import 'package:flame/components.dart';
import '../game_client.dart';

class PowerupComponent extends PositionComponent with HasGameRef<BombermanClient> {
  final String id;
  final double tileSize;
  late SpriteComponent _spriteComponent;

  PowerupComponent({required this.id, required this.tileSize});

  @override
  Future<void> onLoad() async {
    _spriteComponent = SpriteComponent(
      size: Vector2.all(tileSize * 0.8),
      anchor: Anchor.center,
      position: Vector2(tileSize / 2, tileSize / 2)
    );
    add(_spriteComponent);
  }

  void updateState(Map<String, dynamic> data) {
    double x = (data['x'] as num).toDouble();
    double y = (data['y'] as num).toDouble();
    position = Vector2(x * tileSize, y * tileSize);
    
    String type = data['type'];
    String imageName = 'powerup_speed.png'; 
    
    switch(type) {
      case 'fire_up': imageName = 'powerup_fire.png'; break;
      case 'bomb_up': imageName = 'powerup_bomb.png'; break;
      case 'speed_up': imageName = 'powerup_speed.png'; break;
    }
    
    _spriteComponent.sprite = Sprite(gameRef.images.fromCache(imageName));
  }
}