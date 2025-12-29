import 'package:flame/components.dart';
import 'package:flutter/material.dart';

class ExplosionComponent extends PositionComponent {
  final double tileSize;

  ExplosionComponent({required this.tileSize});

  @override
  Future<void> onLoad() async {
    final body = RectangleComponent(
      size: Vector2.all(tileSize),
      paint: Paint()..color = Colors.orange.withValues(alpha: 0.8),
    );
    add(body);
  }

  void updatePosition(int x, int y) {
    position = Vector2(x * tileSize, y * tileSize);
  }
}