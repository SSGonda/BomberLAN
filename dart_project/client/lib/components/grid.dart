import 'package:flame/components.dart';
import 'package:flutter/material.dart';
import '../game_client.dart';

class GridComponent extends PositionComponent with HasGameRef<BombermanClient> {
  final double tileSize;
  List<List<int>>? gridData;
  
  late Sprite hardWallSprite;
  late Sprite softWallSprite;
  late Sprite groundSprite;

  GridComponent({required this.tileSize});

  @override
  Future<void> onLoad() async {
    hardWallSprite = Sprite(gameRef.images.fromCache('wall_hard.png'));
    softWallSprite = Sprite(gameRef.images.fromCache('wall_soft.png'));
    groundSprite = Sprite(gameRef.images.fromCache('floor2.png'));
  }

  void updateGrid(List<List<dynamic>> newGrid) {
    gridData = newGrid.map((row) => row.cast<int>()).toList();
  }

  @override
  void render(Canvas canvas) {
    if (gridData == null) return;

    for (int y = 0; y < gridData!.length; y++) {
      for (int x = 0; x < gridData![y].length; x++) {
        final cellType = gridData![y][x];
        final size = Vector2.all(tileSize);
        final position = Vector2(x * tileSize, y * tileSize);
        
        groundSprite.render(canvas, position: position, size: size);

        if (cellType == 1) { // hardBlock
          hardWallSprite.render(canvas, position: position, size: size);
        } else if (cellType == 2) { // softBlock
          softWallSprite.render(canvas, position: position, size: size);
        }
      }
    }
  }
}