import 'dart:math';

class Powerup {
  String id;
  Point<int> position;
  String type; // 'fire_up', 'bomb_up', 'speed_up'

  Powerup({required this.id, required this.position, required this.type});

  Map<String, dynamic> toJson() {
    return {'id': id, 'x': position.x, 'y': position.y, 'type': type};
  }
}
