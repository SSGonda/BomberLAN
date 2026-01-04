import 'dart:io';
import 'package:args/args.dart';
import 'network/server.dart';
import 'utils/constants.dart';

void main(List<String> arguments) async {
  final parser = ArgParser()
    ..addFlag('host', abbr: 'h', help: 'Run as host')
    ..addOption(
      'port',
      abbr: 'p',
      defaultsTo: GameConstants.defaultPort.toString(),
      help: 'Port to listen on',
    )
    ..addOption(
      'players',
      defaultsTo: GameConstants.defaultMaxPlayers.toString(),
      help: 'Number of players (2-4)',
    )
    ..addOption(
      'duration',
      defaultsTo: GameConstants.defaultGameDuration.toString(),
      help: 'Game duration in seconds (38-600)',
    );

  try {
    final results = parser.parse(arguments);

    if (!results['host']) {
      print('Server mode requires --host flag');
      exit(1);
    }

    final port = int.tryParse(results['port']) ?? GameConstants.defaultPort;
    final maxPlayers =
        int.tryParse(results['players']) ?? GameConstants.defaultMaxPlayers;
    final gameDuration =
        int.tryParse(results['duration']) ?? GameConstants.defaultGameDuration;

    // Validate inputs
    if (maxPlayers < 2 || maxPlayers > 4) {
      print('Number of players must be between 2 and 4');
      exit(1);
    }

    if (gameDuration < GameConstants.minGameDuration ||
        gameDuration > GameConstants.maxGameDuration) {
      print(
        'Game duration must be between ${GameConstants.minGameDuration} and ${GameConstants.maxGameDuration} seconds',
      );
      exit(1);
    }

    if (port < 1 || port > 65535) {
      print('Port must be between 1 and 65535');
      exit(1);
    }

    final server = GameServer(
      port: port,
      maxPlayers: maxPlayers,
      gameDuration: gameDuration,
    );

    server.start();
  } catch (e) {
    print('Error: $e');
    print('\nUsage:');
    print(
      '  dart main.dart --host [--port PORT] [--players NUM] [--duration SECONDS]',
    );
    print('\nExample:');
    print('  dart main.dart --host --port 15800 --players 2 --duration 60');
    exit(1);
  }
}
