import 'dart:convert';
import 'package:flame/game.dart';
import 'package:flame/input.dart';
import 'package:flame/components.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:web_socket_channel/status.dart' as status;

class BombermanClient extends FlameGame with HasKeyboardHandlerComponents {
  late WebSocketChannel channel;
  Map<String, dynamic> lastState = {};

  late Sprite sprPlayer;
  late Sprite sprWallHard;
  late Sprite sprWallSoft;
  late Sprite sprBomb;
  // to add explosion animation and powerups
  TextComponent? timerText;
  TextComponent? gameOverText;

  @override
  Future<void> onLoad() async {
    // load assets 
    sprPlayer = await loadSprite('player.png');
    sprWallHard = await loadSprite('wall_hard.png');
    sprWallSoft = await loadSprite('wall_soft.png');
    sprBomb = await loadSprite('bomb.png');
    // to add explosion animation and powerups

    try {
      channel = WebSocketChannel.connect(Uri.parse(serverUrl));
      channel.stream.listen((message) {
        lastState = jsonDecode(message);
      });
    } catch (e) {
      print("Connection Failed: $e");
    }

    timerText = TextComponent(
      text: '00:00',
      position: Vector2(10, 10),
      priority: 100, 
    );
    add(timerText!);
  }

  void handleSoundEvents(List<dynamic>? events) {
    // Todo when there are sounds
  }
  
}