import 'dart:math';

class Powerup {
  Point<int> position;
  String type;
  bool toRemove;

  Powerup({required this.position, required this.type}) : toRemove = false;

  Map<String, dynamic> toJson() {
    return {'x': position.x, 'y': position.y, 'name': type};
  }
}
