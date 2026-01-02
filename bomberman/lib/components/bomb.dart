import 'package:flame/components.dart';
import 'dart:math';
import '../game_client.dart';

class BombComponent extends PositionComponent with HasGameRef<BombermanClient> {
  final double tileSize;
  double _timer = 0;

  BombComponent({required this.tileSize});

  @override
  Future<void> onLoad() async {
    final sprite = Sprite(gameRef.images.fromCache('bomb.png'));
    
    add(SpriteComponent(
      sprite: sprite,
      size: Vector2.all(tileSize * 0.8),
      anchor: Anchor.center,
      position: Vector2(tileSize / 2, tileSize / 2),
    ));
    priority = 5;
  }

  void updateState(Map<String, dynamic> data) {
    double x = (data['x'] as num).toDouble();
    double y = (data['y'] as num).toDouble();
    position = Vector2(x * tileSize, y * tileSize);
  }
  
  @override
  void update(double dt) {
    super.update(dt);
    _timer += dt * 10;
    scale = Vector2.all(1.0 + 0.05 * sin(_timer));
  }
}