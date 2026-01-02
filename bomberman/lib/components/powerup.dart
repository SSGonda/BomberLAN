import 'package:flame/components.dart';
import '../game_client.dart';

class PowerupComponent extends PositionComponent with HasGameRef<BombermanClient> {
  final double tileSize;
  late SpriteComponent _spriteComponent;

  PowerupComponent({required this.tileSize});

  @override
  Future<void> onLoad() async {
    _spriteComponent = SpriteComponent(
      size: Vector2.all(tileSize * 0.8),
      anchor: Anchor.center,
      position: Vector2(tileSize / 2, tileSize / 2),
    );
    add(_spriteComponent);
    priority = 2;
  }

  void updateState(Map<String, dynamic> data) {
    double x = (data['x'] as num).toDouble();
    double y = (data['y'] as num).toDouble();
    position = Vector2(x * tileSize, y * tileSize);
    
    String type = data['name'] ?? '';
    
    String imageName;
    if (type.contains('fire')) {
      imageName = 'powerup_fire.png';
    } else if (type.contains('speed')) {
      imageName = 'powerup_speed.png';
    } else if (type.contains('bomb')) {
      imageName = 'powerup_bomb.png';
    } else {
      imageName = 'powerup_speed.png';
    }
    
    _spriteComponent.sprite = Sprite(gameRef.images.fromCache(imageName));
  }
}