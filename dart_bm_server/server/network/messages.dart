abstract class GameMessage {
  final String tag;

  GameMessage(this.tag);

  Map<String, dynamic> toJson();

  factory GameMessage.fromJson(Map<String, dynamic> json) {
    final tag = json['tag'] as String;

    switch (tag) {
      case 'player_join':
        return PlayerJoinMessage.fromJson(json);
      case 'player_move':
        return PlayerMoveMessage.fromJson(json);
      case 'plant_bomb':
        return PlantBombMessage.fromJson(json);
      case 'player_quit':
        return PlayerQuitMessage.fromJson(json);
      default:
        throw ArgumentError('Unknown message tag: $tag');
    }
  }
}

class PlayerJoinMessage extends GameMessage {
  PlayerJoinMessage() : super('player_join');

  @override
  Map<String, dynamic> toJson() {
    return {'tag': tag};
  }

  factory PlayerJoinMessage.fromJson(Map<String, dynamic> json) {
    return PlayerJoinMessage();
  }
}

class PlayerMoveMessage extends GameMessage {
  final int playerId;
  final String direction;

  PlayerMoveMessage({required this.playerId, required this.direction})
    : super('player_move');

  @override
  Map<String, dynamic> toJson() {
    return {'tag': tag, 'playerId': playerId, 'direction': direction};
  }

  factory PlayerMoveMessage.fromJson(Map<String, dynamic> json) {
    print('went here');
    return PlayerMoveMessage(
      playerId: json['playerId'] as int,
      direction: json['direction'] as String,
    );
  }
}

class PlantBombMessage extends GameMessage {
  final int playerId;

  PlantBombMessage({required this.playerId}) : super('plant_bomb');

  @override
  Map<String, dynamic> toJson() {
    return {'tag': tag, 'playerId': playerId};
  }

  factory PlantBombMessage.fromJson(Map<String, dynamic> json) {
    return PlantBombMessage(playerId: json['playerId'] as int);
  }
}

class PlayerQuitMessage extends GameMessage {
  final int playerId;

  PlayerQuitMessage({required this.playerId}) : super('player_quit');

  @override
  Map<String, dynamic> toJson() {
    return {'tag': tag, 'playerId': playerId};
  }

  factory PlayerQuitMessage.fromJson(Map<String, dynamic> json) {
    return PlayerQuitMessage(playerId: json['playerId'] as int);
  }
}

class StateUpdateMessage {
  final String tag = 'state_update';
  final Map<String, dynamic> gameState;

  StateUpdateMessage(this.gameState);

  Map<String, dynamic> toJson() {
    return {'tag': tag, 'gameState': gameState};
  }
}
