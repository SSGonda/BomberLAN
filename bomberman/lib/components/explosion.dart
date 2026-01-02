import 'package:flame/components.dart';
import '../game_client.dart';

class ExplosionComponent extends PositionComponent with HasGameRef<BombermanClient> {
  final double tileSize;

  ExplosionComponent({required this.tileSize});

  @override
  Future<void> onLoad() async {
    final sprite = Sprite(gameRef.images.fromCache('explosion.png'));
    
    add(SpriteComponent(
      sprite: sprite,
      size: Vector2.all(tileSize),
      anchor: Anchor.topLeft,
    ));
    priority = 6;
  }

  void updatePosition(int x, int y) {
    position = Vector2(x * tileSize, y * tileSize);
  }
}