import 'dart:math';
import 'dart:math' as math;
import '../utils/constants.dart';
import '../utils/helper.dart';
import 'bomb.dart';

enum CellType { empty, hardBlock, softBlock, powerup, bomb, explosion }

class Grid {
  final int rows;
  final int cols;
  late List<List<CellType>> cells;
  final Random _random = Random();

  Grid({
    this.rows = GameConstants.gridRows,
    this.cols = GameConstants.gridCols,
  }) {
    _initializeGrid();
  }

  void _initializeGrid() {
    cells = List.generate(rows, (_) => List.filled(cols, CellType.empty));

    // Create border with hard blocks
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        if (i == 0 || i == rows - 1 || j == 0 || j == cols - 1) {
          cells[i][j] = CellType.hardBlock;
        } else if (i % 2 == 0 && j % 2 == 0) {
          // Place hard blocks in checkerboard pattern (not on border)
          cells[i][j] = CellType.hardBlock;
        }
      }
    }

    for (int i = 1; i < rows - 1; i++) {
      for (int j = 1; j < cols - 1; j++) {
        if (cells[i][j] == CellType.empty &&
            (i, j) != (1, 1) &&
            (i, j) != (11, 1) &&
            (i, j) != (1, 13) &&
            (i, j) != (11, 13) &&
            _random.nextDouble() < GameConstants.softBlockSpawnChance) {
          cells[i][j] = CellType.softBlock;
        }
      }
    }
  }

  List<Point> getFourCorners(Point<double> position) {
    // find all 4 corners of sprite
    var topLeft = position;
    var topRight = preciseAdd(position, Point(0.8, 0));
    var botLeft = preciseAdd(position, Point(0, 0.8));
    var botRight = preciseAdd(position, Point(0.8, 0.8));
    List<Point> spriteCorners = [topLeft, topRight, botLeft, botRight];
    return spriteCorners;
  }

  bool checkBombWalk(
    Point<double> currentPos,
    Point<double> newPos,
    List<Bomb> bombList,
  ) {
    var prevCorners = getFourCorners(currentPos);
    var newCorners = getFourCorners(newPos);

    //check if inside bomb cell already
    for (final cell in prevCorners) {
      final gridX = cell.x.floor();
      final gridY = cell.y.floor();

      for (final bomb in bombList) {
        if (bomb.position.x == gridX && bomb.position.y == gridY) {
          return true;
        }
      }
    }

    //check if colliding with a bomb
    for (final cell in newCorners) {
      final gridX = cell.x.floor();
      final gridY = cell.y.floor();

      for (final bomb in bombList) {
        if (bomb.position.x == gridX && bomb.position.y == gridY) {
          return false;
        }
      }
    }

    return true;
  }

  bool isWalkable(Point<double> position) {
    var spriteCorners = getFourCorners(position);
    if (position.x <= 0 ||
        position.x >= cols ||
        position.y <= 0 ||
        position.y >= rows) {
      return false;
    }

    // check all 4 corners of sprite
    for (final cell in spriteCorners) {
      final gridX = cell.x.floor();
      final gridY = cell.y.floor();

      final cellType = cells[gridY][gridX];
      if (cellType == CellType.hardBlock || cellType == CellType.softBlock) {
        return false;
      }
    }

    return true;
  }

  bool hasBlock(Point<int> position) {
    final cellType = cells[position.y][position.x];
    return cellType == CellType.hardBlock || cellType == CellType.softBlock;
  }

  bool hasSoftBlock(Point<int> position) {
    return cells[position.y][position.x] == CellType.softBlock;
  }

  bool hasHardBlock(Point<int> position) {
    return cells[position.y][position.x] == CellType.hardBlock;
  }

  void destroySoftBlock(Point<int> position) {
    if (hasSoftBlock(position)) {
      cells[position.y][position.x] = CellType.empty;
    }
  }

  void setCellType(Point<int> position, CellType type) {
    if (position.x >= 0 &&
        position.x < cols &&
        position.y >= 0 &&
        position.y < rows) {
      cells[position.y][position.x] = type;
    }
  }

  CellType getCellType(Point<int> position) {
    return cells[position.y][position.x];
  }

  Point<double> getPlayerStartPosition(int playerNumber, int totalPlayers) {
    // starting positions for players
    final startPositions = [
      Point<double>(1.00, 1.00), // Top-left
      Point<double>(cols - 2.00, 1.00), // Top-right
      Point<double>(1.00, rows - 2.00), // Bottom-left
      Point<double>(cols - 2.00, rows - 2.00), // Bottom-right
    ];

    return startPositions[playerNumber % startPositions.length];
  }

  List<List<int>> toJson() {
    List<List<int>> gridData = cells.map((innerList) {
      return innerList.map((cellType) => cellType.index).toList();
    }).toList();

    return gridData;
  }
}
