import 'dart:math';
import 'dart:async';
import 'package:web_socket_channel/web_socket_channel.dart';
import '../utils/constants.dart';
import 'grid.dart';

class Explosion {
  String id;
  DateTime timePlaced;
  Point<int> position;

  Explosion({required this.id, required this.position})
    : timePlaced = DateTime.now();

  Map<String, dynamic> toJson() {
    return {
      'timePlaced': timePlaced.toIso8601String(),
      'x': position.x,
      'y': position.y,
    };
  }
}

class Bomb {
  String id;
  Point<int> position;
  int playerId;
  int range;
  DateTime plantedTime;
  bool hasExploded;
  Timer? explosionTimer;

  Bomb({
    required this.id,
    required this.position,
    required this.playerId,
    this.range = 1,
  }) : plantedTime = DateTime.now(),
       hasExploded = false;

  void scheduleExplosion(Function onExplode) {
    explosionTimer = Timer(
      const Duration(seconds: GameConstants.bombExplosionTime),
      () {
        if (!hasExploded) {
          hasExploded = true;
          onExplode();
        }
      },
    );
  }

  void cancelTimer() {
    explosionTimer?.cancel();
  }

  List<Explosion> getExplosionCells(String explosionId, Grid grid) {
    final cells = <Explosion>[];

    cells.add(Explosion(id: explosionId, position: position)); // Center cell

    for (int i = 1; i <= range; i++) {
      if (grid.hasHardBlock(Point(position.x, position.y - i))) {
        break;
      }
      // if (!grid.hasHardBlock(Point(position.x, position.y - i))) {
      cells.add(
        Explosion(id: explosionId, position: Point(position.x, position.y - i)),
      );
      // }
    }

    for (int i = 1; i <= range; i++) {
      if (grid.hasHardBlock(Point(position.x, position.y + i))) {
        break;
      }
      // if (!grid.hasHardBlock(Point(position.x, position.y + i))) {
      cells.add(
        Explosion(id: explosionId, position: Point(position.x, position.y + i)),
      );
      // }
    }

    for (int i = 1; i <= range; i++) {
      if (grid.hasHardBlock(Point(position.x - i, position.y))) {
        break;
      }
      // if (!grid.hasHardBlock(Point(position.x - i, position.y))) {
      cells.add(
        Explosion(id: explosionId, position: Point(position.x - i, position.y)),
      );
      // }
    }

    for (int i = 1; i <= range; i++) {
      if (grid.hasHardBlock(Point(position.x + i, position.y))) {
        break;
      }
      // if (!grid.hasHardBlock(Point(position.x + i, position.y))) {
      cells.add(
        Explosion(id: explosionId, position: Point(position.x + i, position.y)),
      );
      // }
    }

    for (final elem in cells) {
      print("${elem.position}, ${elem.id}");
    }

    return cells;
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'x': position.x,
      'y': position.y,
      'playerId': playerId,
      'range': range,
      'plantedTime': plantedTime.toIso8601String(),
    };
  }
}
