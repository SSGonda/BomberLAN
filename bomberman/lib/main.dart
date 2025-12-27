import 'package:flutter/material.dart';
import 'dart:convert';
import 'package:flame/components.dart';
import 'package:flame/game.dart';
import 'package:flame/input.dart';
// import 'package:flame_audio/flame_audio.dart';
import 'package:flutter/services.dart';
import 'package:web_socket_channel/web_socket_channel.dart';

const String serverUrl = 'ws://127.0.0.1:15000'; // connection url
const double tileSize = 40.0; // tile size

void main() {
  runApp(GameWidget(game: BombermanClient()));
}

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

  void _handleSoundEvents(List<dynamic>? events) {
    // Todo when there are sounds
  }
  
}