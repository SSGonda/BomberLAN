import 'dart:math';
import 'dart:collection';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'player.dart';
import 'bomb.dart';
import 'powerup.dart';
import 'grid.dart';
import '../utils/constants.dart';

class GameState {
  final int maxPlayers;
  final int gameDuration;
  final Grid grid;
  final List<Player> players;
  final Map<String, Bomb> bombs;
  final Map<String, Powerup> powerups;
  final List<Explosion> explosions;
  // final Map<String, List<Point<int>>> explosions;
  final Queue<String> playerQueue;
  bool isGameStarted;
  bool isGameOver;
  DateTime gameStartTime;
  String? winner;
  int connectedPlayers;

  GameState({required this.maxPlayers, required this.gameDuration})
    : grid = Grid(),
      players = [],
      bombs = {},
      powerups = {},
      explosions = [],
      playerQueue = Queue(),
      isGameStarted = false,
      isGameOver = false,
      gameStartTime = DateTime.now(),
      connectedPlayers = 0,
      winner = null {
    // Initialize player queue numbers
    for (int i = 1; i <= maxPlayers; i++) {
      playerQueue.add('P$i');
    }
  }

  String? addPlayer(int playerId) {
    if (players.length >= maxPlayers || isGameStarted) {
      return null;
    }

    if (playerQueue.isEmpty) {
      return null;
    }

    final playerNumberStr = playerQueue.removeFirst();
    final playerNumber = players.length;

    final startPosition = grid.getPlayerStartPosition(playerNumber, maxPlayers);

    players.add(Player(id: playerId, startingPosition: startPosition));

    connectedPlayers++;

    // Check if we can start the game
    if (connectedPlayers == maxPlayers) {
      startGame();
    }

    return playerNumberStr;
  }

  void removePlayer(int playerId) {
    final player = players[playerId];
    if (player != null) {
      // Remove player's bombs
      final playerBombs = bombs.values
          .where((bomb) => bomb.playerId == playerId)
          .toList();
      for (final bomb in playerBombs) {
        bomb.cancelTimer();
        bombs.remove(bomb.id);
      }

      // Add player number back to queue
      playerQueue.add('P${player.id}');
      players.removeAt(playerId);
      connectedPlayers--;
    }
  }

  void startGame() {
    isGameStarted = true;
    gameStartTime = DateTime.now();
  }

  void plantBomb(int playerId) {
    final player = players[playerId];
    if (!player.canPlantBomb()) return;

    final bombPosition = Point<int>(
      //changed to round
      player.position.x.round(),
      player.position.y.round(),
    );

    // Check if there's already a bomb at this position
    final existingBomb = bombs.values.firstWhere(
      (bomb) => bomb.position == bombPosition,
      orElse: () => Bomb(id: '', position: Point(0, 0), playerId: playerId),
    );

    if (existingBomb.id.isNotEmpty) return;

    final bombId = 'bomb_${DateTime.now().millisecondsSinceEpoch}_${playerId}';
    final bomb = Bomb(
      id: bombId,
      position: bombPosition,
      playerId: playerId,
      range: player.bombRange,
    );

    bomb.scheduleExplosion(() {
      handleBombExplosion(bombId);
    });

    bombs[bombId] = bomb;
    player.plantBomb();
  }

  void handleBombExplosion(String bombId) {
    final bomb = bombs[bombId];
    if (bomb == null) return;

    final explosionId = 'explosion_${DateTime.now().millisecondsSinceEpoch}';
    final explosionCells = bomb.getExplosionCells(explosionId, grid);

    explosions.addAll(explosionCells);

    // Handle chain reaction
    final bombsToExplode = <Bomb>[];
    for (final cell in explosionCells) {
      // Check for other bombs
      for (final otherBomb in bombs.values) {
        if (otherBomb.id != bombId &&
            otherBomb.position == cell.position &&
            !otherBomb.hasExploded) {
          bombsToExplode.add(otherBomb);
        }
      }

      // Destroy soft blocks
      if (grid.hasSoftBlock(cell.position)) {
        grid.destroySoftBlock(cell.position);

        // if (true) {
        if (Random().nextDouble() < GameConstants.powerupSpawnChance) {
          // final powerupTypes = ['fire_up'];
          final powerupTypes = ['fire_up', 'bomb_up', 'speed_up'];
          final powerupType =
              powerupTypes[Random().nextInt(powerupTypes.length)];
          final powerupId = 'powerup_${DateTime.now().millisecondsSinceEpoch}';

          powerups[powerupId] = Powerup(
            id: powerupId,
            position: cell.position,
            type: powerupType,
          );
        }
      }

      // Check for player collisions
      for (final player in players) {
        if (player.isAlive) {
          final playerCell = Point<int>(
            player.position.x.round(),
            player.position.y.round(),
          );
          if (playerCell == cell.position) {
            player.isAlive = false;
          }
        }
      }
    }

    // Remove the bomb
    bombs.remove(bombId);

    // Handle chain reactions
    for (final bombToExplode in bombsToExplode) {
      bombs.remove(bombToExplode.id);
      bombToExplode.cancelTimer();
      handleBombExplosion(bombToExplode.id);
    }

    // Schedule explosion removal
    Future.delayed(const Duration(seconds: 1), () {
      explosions.removeWhere((explosion) => explosion.id == explosionId);
    });

    // update player bomb count
    final player = players[bomb.playerId];
    if (player != null) {
      player.bombExploded();
    }
  }

  void update(double deltaTime) {
    if (!isGameStarted || isGameOver) return;

    // update player positions
    for (final player in players) {
      if (player.isAlive) {
        if (grid.isWalkable(player.calculateNewPos())) {
          player.updatePosition();
        }

        // explosion collision
        for (final explosion in explosions) {
          if (explosion.position.x == player.position.x.round() &&
              explosion.position.y == player.position.y.round()) {
            player.isAlive = false;
          }
        }

        //  powerup collisions
        final playerCell = Point<int>(
          player.position.x.round(),
          player.position.y.round(),
        );

        final powerupsToRemove = <String>[];
        for (final powerup in powerups.values) {
          if (powerup.position == playerCell) {
            player.addPowerup(powerup.type);
            powerupsToRemove.add(powerup.id);
          }
        }

        for (final powerupId in powerupsToRemove) {
          powerups.remove(powerupId);
        }
      }
    }

    final alivePlayers = players.where((p) => p.isAlive).length;
    if (alivePlayers <= 1 ||
        DateTime.now().difference(gameStartTime).inSeconds > gameDuration) {
      endGame();
    }
  }

  void endGame() {
    isGameOver = true;

    // check winner
    final alivePlayers = players.where((p) => p.isAlive).toList();
    if (alivePlayers.length == 1) {
      winner = 'P${alivePlayers.first.id}';
    } else if (alivePlayers.isNotEmpty) {
      winner = 'draw';
    }
  }

  Map<String, dynamic> toJson() {
    return {
      'maxPlayers': maxPlayers,
      'gameDuration': gameDuration,
      'isGameStarted': isGameStarted,
      'isGameOver': isGameOver,
      'gameStartTime': gameStartTime.toIso8601String(),
      'elapsedTime': DateTime.now().difference(gameStartTime).inSeconds,
      'winner': winner,
      'players': players,
      'bombs': bombs.values.toList(),
      'powerups': powerups.values.toList(),
      'explosions': explosions.map((explosion) => explosion.toJson()).toList(),
      'grid': grid.toJson(),
    };
  }
}
