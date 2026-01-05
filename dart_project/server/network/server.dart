import 'dart:convert';
import 'dart:io';
import 'package:shelf/shelf_io.dart' as io;
import 'package:shelf_web_socket/shelf_web_socket.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:web_socket_channel/status.dart' as status;
import '../game/game_state.dart';
import 'messages.dart';
import '../utils/constants.dart';

class GameServer {
  final int port;
  final int maxPlayers;
  final int gameDuration;
  late GameState gameState;
  final Map<WebSocketChannel, String> clientConnections = {};
  bool isRunning = false;
  DateTime? lastUpdateTime;

  GameServer({
    required this.port,
    required this.maxPlayers,
    required this.gameDuration,
  }) {
    gameState = GameState(maxPlayers: maxPlayers, gameDuration: gameDuration);
  }

  void start() async {
    final handler = webSocketHandler((WebSocketChannel webSocket) {
      webSocket.stream.listen(
        (message) => _handleMessage(webSocket, message),
        onDone: () => _handleDisconnect(webSocket),
        onError: (error) => _handleError(webSocket, error),
      );
    });

    final server = await io.serve(handler, InternetAddress.anyIPv4, port);
    isRunning = true;
    lastUpdateTime = DateTime.now();

    print('Server started on port $port');
    print('Max players: $maxPlayers');
    print('Duration: $gameDuration seconds');

    _startGameLoop();
  }

  void _startGameLoop() {
    Future<void> gameLoop() async {
      while (isRunning) {
        final now = DateTime.now();
        final deltaTime = lastUpdateTime != null
            ? now.difference(lastUpdateTime!).inMilliseconds / 1000.0
            : 0.0;

        // update game state
        gameState.update(deltaTime);

        if (gameState.isGameOver) {
          final updateMessage = StateUpdateMessage(gameState.toJson());
          _broadcastStatusUpdateMessage(updateMessage);
          break;
        }

        if (gameState.isGameStarted && !gameState.isGameOver) {
          final updateMessage = StateUpdateMessage(gameState.toJson());
          _broadcastStatusUpdateMessage(updateMessage);
        }

        lastUpdateTime = now;

        // sleep to maintain tick rate
        await Future.delayed(
          Duration(milliseconds: 1000 ~/ GameConstants.serverTickRate),
        );
      }
    }

    gameLoop();
  }

  void _handleMessage(WebSocketChannel webSocket, dynamic message) {
    try {
      final json = jsonDecode(message);
      final gameMessage = GameMessage.fromJson(json);

      switch (gameMessage.tag) {
        case 'ClientJoin':
          final joinMessage = gameMessage as PlayerJoinMessage;
          _handlePlayerJoin(webSocket, joinMessage);
          break;

        case 'ClientUpdate':
          final clientUpdateMessage = gameMessage as ClientUpdateMessage;
          if (clientUpdateMessage.action == "bomb") {
            final bombMessage = PlantBombMessage.fromJson(json);
            _handlePlantBomb(bombMessage);
            break;
          } else {
            final moveMessage = PlayerMoveMessage.fromJson(json);
            _handlePlayerMove(moveMessage);
            break;
          }
      }
    } catch (e) {
      print('Error handling message: $e');
    }
  }

  void _handlePlayerJoin(
    WebSocketChannel webSocket,
    PlayerJoinMessage message,
  ) {
    if (clientConnections.containsKey(webSocket)) {
      // Already connected
      return;
    }

    if (gameState.isGameStarted) {
      // Game already started, reject connection
      webSocket.sink.add(
        jsonEncode({'tag': 'join_rejected', 'reason': 'Game already started'}),
      );
      return;
    }

    final playerId = gameState.players.length;

    // Add player to game
    print(playerId.runtimeType);
    final playerNumber = gameState.addPlayer(playerId);

    if (playerNumber == null) {
      // Failed to add player
      webSocket.sink.add(
        jsonEncode({'tag': 'join_rejected', 'reason': 'Game is full'}),
      );
      return;
    }

    // store connection
    clientConnections[webSocket] = playerId.toString();

    //  welcome message
    webSocket.sink.add(
      jsonEncode({
        'tag': 'ClientJoin',
        'playerId': playerId,
        'playerNumber': playerNumber,
        'maxPlayers': gameState.maxPlayers,
        'connectedPlayers': gameState.connectedPlayers,
      }),
    );

    // notify all players about new player
    _broadcastMessage({
      'tag': 'player_joined',
      'playerId': playerId,
      'playerNumber': playerNumber,
    });

    // send initial game state
    if (gameState.isGameStarted) {
      final updateMessage = StateUpdateMessage(gameState.toJson());
      webSocket.sink.add(jsonEncode(updateMessage.toJson()));
    }
  }

  void _handlePlayerMove(PlayerMoveMessage message) {
    final player = gameState.players[message.playerId];
    if (player.isAlive && gameState.isGameStarted && !gameState.isGameOver) {
      player.move(message.direction);
    }
  }

  void _handlePlantBomb(PlantBombMessage message) {
    if (gameState.isGameStarted && !gameState.isGameOver) {
      gameState.plantBomb(message.playerId);
    }
  }

  void _handleDisconnect(WebSocketChannel webSocket) {
    final playerId = int.parse(clientConnections[webSocket]!);
    if (playerId != null) {
      gameState.removePlayer(playerId);
      clientConnections.remove(webSocket);

      _broadcastMessage({'tag': 'player_left', 'playerId': playerId});
    }
  }

  void _handleError(WebSocketChannel webSocket, dynamic error) {
    print('WebSocket error: $error');
    _handleDisconnect(webSocket);
  }

  void _broadcastMessage(Map<String, dynamic> message) {
    final jsonMessage = jsonEncode(message);
    for (final connection in clientConnections.keys) {
      try {
        connection.sink.add(jsonMessage);
      } catch (e) {
        print('Error broadcasting message: $e');
      }
    }
  }

  void _broadcastStatusUpdateMessage(StateUpdateMessage message) {
    final jsonMessage = jsonEncode(message.toJson());
    for (final connection in clientConnections.keys) {
      try {
        connection.sink.add(jsonMessage);
      } catch (e) {
        print('Error broadcasting message: $e');
      }
    }
  }

  void stop() {
    isRunning = false;
    for (final connection in clientConnections.keys) {
      connection.sink.close(status.goingAway);
    }
    clientConnections.clear();
  }
}
