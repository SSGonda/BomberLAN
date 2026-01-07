import 'package:flame/game.dart';
import 'package:flutter/material.dart';
import 'game_client.dart';

void main() {
  const String ip = String.fromEnvironment('IP', defaultValue: '127.0.0.1');
  const int port = int.fromEnvironment('PORT', defaultValue: 15800);

  print(port);
  runApp(
    GameWidget(
      game: BombermanClient(serverIp: ip, serverPort: port),
    ),
  );
}