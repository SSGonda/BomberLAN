import 'dart:convert';
import 'package:web_socket_channel/web_socket_channel.dart';
import '../shared/messages.dart';

class NetworkController {
  late WebSocketChannel channel;
  Function(Map<String, dynamic>)? onGameStateUpdate;
  Function(int, int)? onJoinAcc; // contains playerId and playerNumber

  void connect(String ip, int port) {
    // connect to the websocket
    final uri = Uri.parse('ws://$ip:$port');
    channel = WebSocketChannel.connect(uri);

    // listening for messages
    channel.stream.listen(
      (message) {
        handleMessage(message);
      },
      onError: (error) => print('Connection Error: $error'),
      onDone: () => print('Connection Closed'),
    );

    // join request
    send(PlayerJoinMessage()); 
  }

  void handleMessage(dynamic message) {
    final json = jsonDecode(message);
    
    // identify message type
    final tag = json['tag'];

    switch (tag) {
      case 'join_accepted':
        if (onJoinAcc != null) {
          onJoinAcc!(json['playerId'], json['playerNumber']);
        }
        break;
      case 'state_update':
        if (onGameStateUpdate != null) {
          onGameStateUpdate!(json['gameState']);
        }
        break;
      // TODO: add code for handling other messages in the future
    }
  }

  void send(GameMessage message) {
    channel.sink.add(jsonEncode(message.toJson()));
  }

  void dispose() {
    channel.sink.close();
  }
}