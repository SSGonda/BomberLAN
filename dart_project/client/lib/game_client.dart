import 'dart:convert';
import 'package:flame/game.dart';
import 'package:flame/input.dart';
import 'package:flame/components.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:flame_audio/flame_audio.dart';

import 'components/grid.dart';
import 'components/player.dart';
import 'components/bomb.dart';
import 'components/explosion.dart';
import 'components/powerup.dart';
import 'components/hud.dart';

class BombermanClient extends FlameGame with HasKeyboardHandlerComponents {
  final String serverIp;
  final int serverPort;
  late WebSocketChannel channel;

  int? myPlayerId;
  
  late GridComponent gridComponent;
  late HudComponent hudComponent;
  final Map<int, PlayerComponent> _players = {};
  final Map<String, BombComponent> _bombs = {};
  final Map<String, ExplosionComponent> _explosions = {};
  final Map<String, PowerupComponent> _powerups = {};

  static const double tileSize = 40.0;

  final Set<String> _playedExplosionIds = {};
  final Map<int, bool> _previousAliveState = {};
  final Map<int, Map<String, num>> _previousPlayerStats = {};
  bool _gameOverSoundPlayed = false;

  BombermanClient({required this.serverIp, required this.serverPort});

  @override
  Future<void> onLoad() async {
    await images.loadAll([
      'player1.png', 'player2.png', 'player3.png', 'player4.png',
      'wall_hard.png', 'wall_soft.png', 'floor2.png',
      'bomb.png', 'explosion.png',
      'powerup_fire.png', 'powerup_bomb.png', 'powerup_speed.png'
    ]);

    await FlameAudio.audioCache.loadAll([
    'explosion.mp3',
    'death.mp3',
    'powerup.mp3',
    'win.mp3',
    'lose.mp3',
    'draw.mp3',
  ]);

    camera.viewfinder.anchor = Anchor.topLeft;

    gridComponent = GridComponent(tileSize: tileSize);
    hudComponent = HudComponent();
    
    add(gridComponent);
    add(hudComponent);

    _connectToServer();
  }

  void _connectToServer() {
    final uri = Uri.parse('ws://$serverIp:$serverPort');
    try {
      channel = WebSocketChannel.connect(uri);
      
      _sendJson({'tag': 'ClientJoin'});

      channel.stream.listen(
        (message) => _handleMessage(message),
        onError: (e) => print('WS Error: $e'),
        onDone: () => print('WS Closed'),
      );
    } catch (e) {
      print('Connection failed: $e');
    }
  }

  void _handleMessage(dynamic message) {
    final json = jsonDecode(message);
    final tag = json['tag'];

    switch (tag) {
      case 'ClientJoin':
        myPlayerId = json['playerId'];
        print('Joined as Player: ${myPlayerId! + 1}');
        break;
      case 'StateUpdate':
        _updateGameState(json['gameState']);
        break;
      case 'join_rejected':
        print('Join Rejected: ${json['reason']}');
        break;
      case 'player_joined':
        print('Player ${json['playerId'] + 1} joined');
        break;
      case 'player_left':
        print('Player ${json['playerId'] + 1} left');
        break;
    }
  }

  void _updateGameState(Map<String, dynamic> state) {

    // audio
    final serverExplosions = List<dynamic>.from(state['explosions']);
    final currentExplosionIds = <String>{};

    for (var eData in serverExplosions) {
      final id = eData['id'] as String? ?? "${eData['x']}_${eData['y']}_${eData['timePlaced']}";
      
      currentExplosionIds.add(id);

      if (!_playedExplosionIds.contains(id)) {
        FlameAudio.play('explosion.mp3');
        _playedExplosionIds.add(id);
      }
    }
    _playedExplosionIds.removeWhere((id) => !currentExplosionIds.contains(id));

    final serverPlayers = List<dynamic>.from(state['players']);
    
    for (var pData in serverPlayers) {
      final pid = pData['id'] as int;
      final isAlive = pData['isAlive'] as bool;
      
      // death
      if ((_previousAliveState[pid] ?? true) && !isAlive) {
        FlameAudio.play('death.mp3');
      }
      _previousAliveState[pid] = isAlive;

      // powerup pickup
      final currentStats = {
        'range': pData['bombRange'] as num,
        'maxBombs': pData['maxBombs'] as num,
        'speed': pData['speed'] as num,
      };

      if (_previousPlayerStats.containsKey(pid)) {
        final prev = _previousPlayerStats[pid]!;
        if (currentStats['range']! > prev['range']! || 
            currentStats['maxBombs']! > prev['maxBombs']! ||
            currentStats['speed']! > prev['speed']!) {
          FlameAudio.play('powerup.mp3');
        }
      }
      _previousPlayerStats[pid] = currentStats;
    }

    // win lose draw
    final winner = state['winner'];
    
    if (winner != null && !_gameOverSoundPlayed && myPlayerId != null) {
      _gameOverSoundPlayed = true;

      if (winner == 'draw') {
        FlameAudio.play('draw.mp3');
      } else {
        final winnerId = int.tryParse(winner.toString().replaceAll(RegExp(r'[^0-9]'), ''));
        
        if (winnerId == myPlayerId) {
          FlameAudio.play('win.mp3');
        } else {
          FlameAudio.play('lose.mp3');
        }
      }
    }

    // grid
    final gridData = List<List<dynamic>>.from(state['grid']);
    gridComponent.updateGrid(gridData);

    // HUD
    hudComponent.updateTime(state['elapsedTime'], state['gameDuration']);
    if (state['isGameOver'] == true) {
      hudComponent.showGameOver(state['winner']);
    }

    // players
    // final serverPlayers = List<dynamic>.from(state['players']);
    final activeIds = <int>{};
    for (var pData in serverPlayers) {
      final pid = pData['id'] as int;
      activeIds.add(pid);
      
      if (!_players.containsKey(pid)) {
        final p = PlayerComponent(id: pid, tileSize: tileSize);
        _players[pid] = p;
        add(p);
      }
      _players[pid]!.updateState(pData);
    }
    _players.removeWhere((id, component) {
      if (!activeIds.contains(id)) {
        remove(component);
        return true;
      }
      return false;
    });

    // Bombs
    final serverBombs = List<dynamic>.from(state['bombs']);
    final activeBombKeys = <String>{};
    
    for (var bData in serverBombs) {
      final key = "${bData['x']}_${bData['y']}";
      activeBombKeys.add(key);
      
      if (!_bombs.containsKey(key)) {
        final b = BombComponent(tileSize: tileSize);
        _bombs[key] = b;
        add(b);
      }
      _bombs[key]!.updateState(bData);
    }
    _bombs.removeWhere((key, component) {
      if (!activeBombKeys.contains(key)) {
        remove(component);
        return true;
      }
      return false;
    });

    // explosions
    // final serverExplosions = List<dynamic>.from(state['explosions']);
    final activeExpKeys = <String>{};
    
    for (var eData in serverExplosions) {
      final key = "${eData['x']}_${eData['y']}";
      activeExpKeys.add(key);

      if (!_explosions.containsKey(key)) {
        final e = ExplosionComponent(tileSize: tileSize);
        _explosions[key] = e;
        add(e);
      }
      _explosions[key]!.updatePosition(eData['x'], eData['y']);
    }
    _explosions.removeWhere((key, component) {
      if (!activeExpKeys.contains(key)) {
        remove(component);
        return true;
      }
      return false;
    });

    // powerups
    final serverPowerups = List<dynamic>.from(state['powerups']);
    final activePowerupKeys = <String>{};
    
    for (var pData in serverPowerups) {
      final key = "${pData['x']}_${pData['y']}";
      activePowerupKeys.add(key);
      
      if (!_powerups.containsKey(key)) {
        final p = PowerupComponent(tileSize: tileSize);
        _powerups[key] = p;
        add(p);
      }
      _powerups[key]!.updateState(pData);
    }
    _powerups.removeWhere((key, component) {
      if (!activePowerupKeys.contains(key)) {
        remove(component);
        return true;
      }
      return false;
    });
  }

  void _sendJson(Map<String, dynamic> data) {
    if (channel.closeCode == null) {
      channel.sink.add(jsonEncode(data));
    }
  }

  @override
  KeyEventResult onKeyEvent(KeyEvent event, Set<LogicalKeyboardKey> keysPressed) {
    if (myPlayerId == null) return KeyEventResult.ignored;

    if (event is KeyDownEvent || event is KeyRepeatEvent) {
      String? action;
      if (keysPressed.contains(LogicalKeyboardKey.arrowUp)) action = 'up';
      else if (keysPressed.contains(LogicalKeyboardKey.arrowDown)) action = 'down';
      else if (keysPressed.contains(LogicalKeyboardKey.arrowLeft)) action = 'left';
      else if (keysPressed.contains(LogicalKeyboardKey.arrowRight)) action = 'right';
      else if (keysPressed.contains(LogicalKeyboardKey.space)) action = 'bomb';

      if (action != null) {
        _sendJson({
          'tag': 'ClientUpdate',
          'playerId': myPlayerId,
          'action': action
        });
        return KeyEventResult.handled;
      }
    }
    
    if (event is KeyUpEvent) {
       if (!keysPressed.contains(LogicalKeyboardKey.arrowUp) &&
           !keysPressed.contains(LogicalKeyboardKey.arrowDown) &&
           !keysPressed.contains(LogicalKeyboardKey.arrowLeft) &&
           !keysPressed.contains(LogicalKeyboardKey.arrowRight)) {
         
         _sendJson({
           'tag': 'ClientUpdate',
           'playerId': myPlayerId,
           'action': 'stop'
         });
       }
    }
    return KeyEventResult.ignored;
  }
}