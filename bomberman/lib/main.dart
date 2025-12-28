import 'package:flutter/material.dart';
import 'dart:convert';
import 'package:flame/components.dart';
import 'package:flame/game.dart';
import 'package:flame/input.dart';
// import 'package:flame_audio/flame_audio.dart';
import 'package:flutter/services.dart';
import 'package:web_socket_channel/web_socket_channel.dart';

const String serverUrl = 'ws://127.0.0.1:15000'; // connection url
const double tileSize = 40.0; // tile size

abstract class GameGrid extends SpriteComponent {
  GameGrid({required Sprite sprite, required Vector2 position})
      : super(sprite: sprite, size: Vector2.all(tileSize), position: position);
}

class HardBlock extends GameGrid {
  HardBlock(Sprite sprite, Vector2 position) : super(sprite: sprite, position: position);
}

class SoftBlock extends GameGrid {
  SoftBlock(Sprite sprite, Vector2 position) : super(sprite: sprite, position: position);
}

class Floor extends GameGrid {
  Floor(Sprite sprite, Vector2 position) : super(sprite: sprite, position: position);
}

class BombComponent extends GameGrid {
  final String id;
  BombComponent(this.id, Sprite sprite, Vector2 position)
      : super(sprite: sprite, position: position);
}

class PowerupComponent extends GameGrid {
  final String id;
  PowerupComponent(this.id, Sprite sprite, Vector2 position)
      : super(sprite: sprite, position: position);
}

class ExplosionComponent extends GameGrid {
  final String id;
  ExplosionComponent(this.id, Sprite sprite, Vector2 position)
      : super(sprite: sprite, position: position);
}

class PlayerComponent extends SpriteComponent {
  final int playerId;
  
  late TextComponent label;

  PlayerComponent({
    required this.playerId,
    required Sprite sprite,
    required Vector2 position,
  }) : super(sprite: sprite, size: Vector2.all(tileSize * 0.8), position: position) {
    anchor = Anchor.center; 
    
    label = TextComponent(
      text: 'P${playerId + 1}',
      textRenderer: TextPaint(
        style: const TextStyle(fontSize: 10, color: Colors.white, fontWeight: FontWeight.bold),
      ),
    );
    label.position = Vector2(size.x / 2 - 5, -12);
    add(label);
  }

  // move to new position
  void updateTargetPosition(double serverX, double serverY) {
    position = Vector2(serverX * tileSize, serverY * tileSize);
  }
}
void main() {
  runApp(GameWidget(game: BombermanClient()));
}

class BombermanClient extends FlameGame with HasKeyboardHandlerComponents {
  late WebSocketChannel channel;
  Map<String, dynamic> lastState = {};

  late Sprite sprPlayer;
  late Sprite sprWallHard;
  late Sprite sprWallSoft;
  late Sprite sprBomb;
  // to add explosion animation and powerups
  TextComponent? timerText;
  TextComponent? gameOverText;

  @override
  Future<void> onLoad() async {
    // load assets 
    sprPlayer = await loadSprite('player.png');
    sprWallHard = await loadSprite('wall_hard.png');
    sprWallSoft = await loadSprite('wall_soft.png');
    sprBomb = await loadSprite('bomb.png');
    // to add explosion animation and powerups

    try {
      channel = WebSocketChannel.connect(Uri.parse(serverUrl));
      channel.stream.listen((message) {
        lastState = jsonDecode(message);
      });
    } catch (e) {
      print("Connection Failed: $e");
    }

    timerText = TextComponent(
      text: '00:00',
      position: Vector2(10, 10),
      priority: 100, 
    );
    add(timerText!);
  }

  void _handleSoundEvents(List<dynamic>? events) {
    // Todo when there are sounds
  }
  
}