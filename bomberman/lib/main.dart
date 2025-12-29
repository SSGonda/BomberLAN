import 'package:flame/game.dart';
import 'package:flutter/material.dart';
import 'game_client.dart';

void main(List<String> args) {
  String ip = '127.0.0.1';
  int port = 15800; // Default port from constants

  // Simple argument parsing for IP and Port
  if (args.length >= 2) {
    ip = args[0];
    port = int.tryParse(args[1]) ?? 15800;
  }

  runApp(
    GameWidget(
      game: BombermanClient(serverIp: ip, serverPort: port),
    ),
  );
}