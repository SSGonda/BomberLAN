# CS:GO

## Dart
Using flame and websockets

### How to Run

#### Server
To start the server,

Navigate to `dart_project/server`

Run `dart main.dart --host --port <port> --players <players> --duration <game duration>`

As an example, the command for starting the server using the provided port in the project documentation while having 2 players and a duration of 60 seconds would be:
`dart main.dart --host --port 15800 --players 2 --duration 60`

#### Client

To start a client,

Navigate to `dart_project/client`

Run `flutter run --dart-entrypoint-args="127.0.0.1 15800"`

This should run a client that connects to `127.0.0.1` in port `15000`




## Haskell
Using miso and websockets

### How to Run

#### Server

To start the server,

Navigate to `haskell-project`

Run `cabal run haskell -- <players> <game duration> --host <port>`

Where players, game duration and port are integers without the `<>`

The server should be started in `127.0.0.1:<port>`

#### Client

To start a client,

Navigate to `haskell-project`

Run `cabal run`

This should run a client that connects to `127.0.0.1` in port `15000`

## References

### Dart

Flame Engine Documentation [https://docs.flame-engine.org/latest/](https://docs.flame-engine.org/latest/)

### Haskell

Server Boilerplate Code from [https://jaspervdj.be/websockets/example/server.html](https://jaspervdj.be/websockets/example/server.html)

Client Boilerplate Code from [https://github.com/haskell-miso/miso-websocket/blob/main/src/WebSocket.hs](https://github.com/haskell-miso/miso-websocket/blob/main/src/WebSocket.hs)
