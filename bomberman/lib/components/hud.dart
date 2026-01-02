import 'package:flame/components.dart';
import 'package:flutter/material.dart';

class HudComponent extends PositionComponent {
  late TextComponent _timerText;
  late TextComponent _statusText;

  @override
  Future<void> onLoad() async {
    _timerText = TextComponent(
      text: 'Time: 00:00',
      textRenderer: TextPaint(
        style: const TextStyle(
          color: Colors.white, 
          fontSize: 24, 
          fontWeight: FontWeight.bold,
          shadows: [Shadow(blurRadius: 4, color: Colors.black)]
        )
      ),
      position: Vector2(20, 10),
    );
    
    _statusText = TextComponent(
      text: '',
      textRenderer: TextPaint(
        style: const TextStyle(
          color: Colors.red, 
          fontSize: 48, 
          fontWeight: FontWeight.bold,
          shadows: [Shadow(blurRadius: 10, color: Colors.black)]
        )
      ),
      anchor: Anchor.center,
    );

    add(_timerText);
    add(_statusText);
    _statusText.position = Vector2(300, 260);
    priority = 100;
  }

  void updateTime(int elapsed, int duration) {
    int remaining = duration - elapsed;
    if (remaining < 0) remaining = 0;
    
    String minutes = (remaining ~/ 60).toString().padLeft(2, '0');
    String seconds = (remaining % 60).toString().padLeft(2, '0');
    
    _timerText.text = '$minutes:$seconds';
  }

  void showGameOver(String? winner) {
    if (winner == 'draw') {
      _statusText.text = 'DRAW!';
    } else if (winner != null) {
      _statusText.text = '$winner WINS!';
    } else {
      _statusText.text = 'GAME OVER';
    }
  }
}