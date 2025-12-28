import 'dart:math';
import '../utils/constants.dart';
import '../utils/helper.dart';

class Player {
  int id;
  Point<double> position;
  Point<double> velocity;
  List<String> activePowerups;
  int maxBombs;
  int currentBombs;
  int bombRange;
  double speed;
  bool isAlive;
  // DateTime lastTeleportTime;

  Player({required this.id, required Point<double> startingPosition})
    : position = startingPosition,
      velocity = Point(0, 0),
      activePowerups = [],
      maxBombs = 1,
      currentBombs = 0,
      bombRange = 1,
      speed = GameConstants.baseSpeed,
      isAlive = true;
  // lastTeleportTime = DateTime.now().subtract(const Duration(seconds: 2));

  void move(String direction) {
    switch (direction) {
      case 'up':
        velocity = Point(0, -speed);
        break;
      case 'down':
        velocity = Point(0, speed);
        break;
      case 'left':
        velocity = Point(-speed, 0);
        break;
      case 'right':
        velocity = Point(speed, 0);
        break;
      case 'stop':
        velocity = Point(0, 0);
        break;
    }
  }

  Point<double> calculateNewPos() {
    if (velocity.x < 0 || velocity.y < 0) {
      if (velocity.x < 0) {
        var newPos = preciseAdd(
          position,
          (velocity - Point(GameConstants.playerWidth, 0)),
        );
        return newPos;
      } else {
        var newPos = preciseAdd(
          position,
          (velocity - Point(0, GameConstants.playerWidth)),
        );
        return newPos;
      }
    } else {
      if (velocity.x > 0) {
        var newPos = preciseAdd(
          position,
          (velocity + Point(GameConstants.playerWidth, 0)),
        );
        return newPos;
      } else {
        var newPos = preciseAdd(
          position,
          (velocity + Point(0, GameConstants.playerWidth)),
        );
        return newPos;
      }
    }
  }

  void updatePosition() {
    var newPos = preciseAdd(position, (velocity));

    position = newPos;
    // print(position);
  }

  bool canPlantBomb() {
    return currentBombs < maxBombs;
  }

  void plantBomb() {
    currentBombs++;
  }

  void bombExploded() {
    currentBombs--;
  }

  void addPowerup(String powerupType) {
    switch (powerupType) {
      case 'fire_up':
        bombRange++;
        break;
      case 'bomb_up':
        maxBombs++;
        break;
      case 'speed_up':
        speed += GameConstants.speedUpIncrement;
        break;
    }
    activePowerups.add(powerupType);
  }

  void removePowerup(String powerupType) {
    activePowerups.remove(powerupType);
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'x': position.x,
      'y': position.y,
      'deltaX': velocity.x,
      'deltaY': velocity.y,
      'maxBombs': maxBombs,
      'currentBombs': currentBombs,
      'bombRange': bombRange,
      'speed': speed,
      'isAlive': isAlive,
      'activePowerups': activePowerups,
    };
  }
}
