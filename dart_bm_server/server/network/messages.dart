abstract class GameMessage {
  final String tag;

  GameMessage(this.tag);

  Map<String, dynamic> toJson();

  factory GameMessage.fromJson(Map<String, dynamic> json) {
    final tag = json['tag'] as String;

    switch (tag) {
      case 'ClientJoin':
        return PlayerJoinMessage.fromJson(json);
      case 'ClientUpdate':
        return ClientUpdateMessage.fromJson(json);
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
  PlayerJoinMessage() : super('ClientJoin');

  @override
  Map<String, dynamic> toJson() {
    return {'tag': tag};
  }

  factory PlayerJoinMessage.fromJson(Map<String, dynamic> json) {
    return PlayerJoinMessage();
  }
}

class ClientUpdateMessage extends GameMessage {
  final int playerId;
  final String action;

  ClientUpdateMessage({required this.playerId, required this.action})
    : super('ClientUpdate');

  @override
  Map<String, dynamic> toJson() {
    return {'tag': tag, 'playerId': playerId, 'action': action};
  }

  factory ClientUpdateMessage.fromJson(Map<String, dynamic> json) {
    return ClientUpdateMessage(
      playerId: json['playerId'] as int,
      action: json['action'] as String,
    );
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
      direction: json['action'] as String,
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
  final String tag = 'StateUpdate';
  final Map<String, dynamic> gameState;

  StateUpdateMessage(this.gameState);

  Map<String, dynamic> toJson() {
    return {'tag': tag, 'gameState': gameState};
  }
}
