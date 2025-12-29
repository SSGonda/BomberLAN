import 'dart:math';
import 'dart:math' as math;
import '../utils/constants.dart';

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

    // Add soft blocks (Phase 4: 40% chance per free cell)
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

  bool isWalkable(Point<double> position) {
    final gridX = position.x.floor();
    final gridY = position.y.floor();

    // print("new pos in is walkable: $position");

    if (position.x <= 0 ||
        position.x >= cols ||
        position.y <= 0 ||
        position.y >= rows) {
      // print("not walkable");
      return false;
    }

    final cellType = cells[gridY][gridX];
    return cellType == CellType.empty || cellType == CellType.powerup;
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
      Point<double>(1.50, 1.50), // Top-left
      Point<double>(cols - 1.50, 1.50), // Top-right
      Point<double>(1.50, rows - 1.50), // Bottom-left
      Point<double>(cols - 1.50, rows - 1.50), // Bottom-right
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
