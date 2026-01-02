import 'dart:math';

class Powerup {
  Point<int> position;
  String type; // 'fire_up', 'bomb_up', 'speed_up'

  Powerup({required this.position, required this.type});

  Map<String, dynamic> toJson() {
    return {'x': position.x, 'y': position.y, 'type': type};
  }
}
