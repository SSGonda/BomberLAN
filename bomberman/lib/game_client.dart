import 'dart:convert';
import 'package:flame/game.dart';
import 'package:flame/input.dart';
import 'package:flame/components.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:web_socket_channel/web_socket_channel.dart';

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
  Map<String, dynamic>? lastGameState;
  
  late GridComponent gridComponent;
  late HudComponent hudComponent;
  final Map<int, PlayerComponent> _players = {};
  final Map<String, BombComponent> _bombs = {};
  final Map<String, ExplosionComponent> _explosions = {};
  final Map<String, PowerupComponent> _powerups = {};

  static const double tileSize = 40.0;
  
  BombermanClient({required this.serverIp, required this.serverPort});

  @override
  Future<void> onLoad() async {
    await images.loadAll([
    'player.png', 'player1.png', 'player2.png', 'player3.png', 'player4.png', 
    'wall_hard.png', 'wall_soft.png', 'floor2.png',
    'bomb.png', 'explosion.png',
    'powerup_fire.png', 'powerup_bomb.png', 'powerup_speed.png'
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
    print('Connecting to $uri');
    
    try {
      channel = WebSocketChannel.connect(uri);
      
      _sendJson({'tag': 'ClientJoin'});

      channel.stream.listen(
        (message) {
          _handleMessage(message);
        },
        onError: (error) => print('WS Error: $error'),
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
        print('Joined as Player ID: $myPlayerId');
        break;
        
      case 'StateUpdate':
        _updateGameState(json['gameState']);
        break;
        
      case 'join_rejected':
        print('Join Rejected: ${json['reason']}');
        break;
    }
  }

  void _updateGameState(Map<String, dynamic> state) {
    lastGameState = state;

    // grid
    final gridData = List<List<dynamic>>.from(state['grid']);
    gridComponent.updateGrid(gridData);

    // HUD
    hudComponent.updateTime(state['elapsedTime'], state['gameDuration']);
    if (state['isGameOver'] == true) {
      hudComponent.showGameOver(state['winner']);
    }

    // players
    final serverPlayers = List<dynamic>.from(state['players']);
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

    // bombs
    final serverBombs = List<dynamic>.from(state['bombs']);
    final activeBombs = <String>{};
    
    for (var bData in serverBombs) {
      final bid = bData['id'] as String;
      activeBombs.add(bid);
      
      if (!_bombs.containsKey(bid)) {
        final b = BombComponent(id: bid, tileSize: tileSize);
        _bombs[bid] = b;
        add(b);
      }
      _bombs[bid]!.updateState(bData);
    }
    _bombs.removeWhere((id, component) {
      if (!activeBombs.contains(id)) {
        remove(component);
        return true;
      }
      return false;
    });

    // explosions
    final serverExplosions = List<dynamic>.from(state['explosions']);
    final activeExpIds = <String>{}; 
    
    final currentExplosionKeys = <String>{};
    for (var eData in serverExplosions) {
      final key = "${eData['x']}_${eData['y']}";
      currentExplosionKeys.add(key);

      if (!_explosions.containsKey(key)) {
        final e = ExplosionComponent(tileSize: tileSize);
        _explosions[key] = e;
        add(e);
      }
      _explosions[key]!.updatePosition(eData['x'], eData['y']);
    }
    _explosions.removeWhere((key, component) {
      if (!currentExplosionKeys.contains(key)) {
        remove(component);
        return true;
      }
      return false;
    });
    
    // powerups
    final serverPowerups = List<dynamic>.from(state['powerups']);
    final activePowerups = <String>{};
    
    for (var pData in serverPowerups) {
      final pid = pData['id'] as String;
      activePowerups.add(pid);
      
      if (!_powerups.containsKey(pid)) {
        final p = PowerupComponent(id: pid, tileSize: tileSize);
        _powerups[pid] = p;
        add(p);
      }
      _powerups[pid]!.updateState(pData);
    }
    _powerups.removeWhere((id, component) {
      if (!activePowerups.contains(id)) {
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
      // movement
      String? direction;
      if (keysPressed.contains(LogicalKeyboardKey.arrowUp)) direction = 'up';
      else if (keysPressed.contains(LogicalKeyboardKey.arrowDown)) direction = 'down';
      else if (keysPressed.contains(LogicalKeyboardKey.arrowLeft)) direction = 'left';
      else if (keysPressed.contains(LogicalKeyboardKey.arrowRight)) direction = 'right';
      
      if (direction != null) {
        _sendJson({
          'tag': 'ClientUpdate',
          'playerId': myPlayerId,
          'action': direction
        });
        return KeyEventResult.handled;
      }

      // bomb
      if (keysPressed.contains(LogicalKeyboardKey.space)) {
        _sendJson({
          'tag': 'ClientUpdate',
          'playerId': myPlayerId,
          'action': 'bomb'
        });
        return KeyEventResult.handled;
      }
    }
    
    // stop handler
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