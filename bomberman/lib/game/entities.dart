import 'package:flame/components.dart';
import 'package:flutter/material.dart';

const double tileSize = 40.0;

class GameGrid extends SpriteComponent {
  GameGrid(Sprite sprite, Vector2 position, {ComponentKey? key})
      : super(sprite: sprite, size: Vector2.all(tileSize), position: position, key: key);
}
class HardBlock extends GameGrid {
  HardBlock(Sprite sprite, Vector2 position) : super(sprite, position);
}

class SoftBlock extends GameGrid {
  SoftBlock(Sprite sprite, Vector2 position, {ComponentKey? key}) 
      : super(sprite, position, key: key);
}

class Floor extends GameGrid {
  Floor(Sprite sprite, Vector2 position) : super(sprite, position);
}

class BombComponent extends GameGrid {
  final String id;
  BombComponent(this.id, Sprite sprite, Vector2 position) : super(sprite, position);
}

class ExplosionComponent extends GameGrid {
  ExplosionComponent(Sprite sprite, Vector2 position) : super(sprite, position);
}

class PowerupComponent extends GameGrid {
  final String id;
  PowerupComponent(this.id, Sprite sprite, Vector2 position) : super(sprite, position);
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
      text: 'P${(playerId % 4) + 1}', 
      textRenderer: TextPaint(
        style: const TextStyle(
          fontSize: 10, 
          color: Colors.white, 
          fontWeight: FontWeight.bold,
          shadows: [Shadow(blurRadius: 2, color: Colors.black)],
        ),
      ),
    );
    label.position = Vector2(size.x / 2 - 5, -15);
    add(label);
  }

  void updateTargetPosition(double serverX, double serverY) {
    position = Vector2((serverX + 0.5) * tileSize, (serverY + 0.5) * tileSize);
  }
}