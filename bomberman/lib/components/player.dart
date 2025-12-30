import 'package:flame/components.dart';
import 'package:flutter/material.dart';
import '../game_client.dart';

class PlayerComponent extends PositionComponent with HasGameRef<BombermanClient> {
  final int id;
  final double tileSize;
  
  late SpriteComponent _sprite;
  late TextComponent _label;

  PlayerComponent({required this.id, required this.tileSize});

  @override
  Future<void> onLoad() async {
    final spriteImage = gameRef.images.fromCache('player${id + 1}.png');
    
    _sprite = SpriteComponent(
      sprite: Sprite(spriteImage),
      size: Vector2.all(tileSize * 0.8),
      anchor: Anchor.center,
      position: Vector2(tileSize / 2, tileSize / 2),
    );
    
    _label = TextComponent(
      text: 'P${id + 1}',
      textRenderer: TextPaint(style: const TextStyle(color: Colors.white, fontSize: 10, fontWeight: FontWeight.bold)),
      anchor: Anchor.center,
      position: Vector2(tileSize / 2, -5), 
    );

    add(_sprite);
    add(_label);
    
    size = Vector2.all(tileSize);
  }

  void updateState(Map<String, dynamic> data) {
    double x = (data['x'] as num).toDouble();
    double y = (data['y'] as num).toDouble();
    bool isAlive = data['isAlive'];

    position = Vector2(x * tileSize, y * tileSize);
    
    if (!isAlive) {
      _sprite.paint.color = Colors.grey;
      _sprite.angle = 3.14159;
    } else {
       _sprite.paint.color = const Color(0xFFFFFFFF);
       _sprite.angle = 0;
    }
  }
}