# CS:GO

## Dart
Using flame and websockets

## Haskell
Using miso and websockets

### How to Run

#### Server

To start the server,

Navigate to `haskell`

Run `cabal run haskell -- <players> <game duration> --host <port>`

Where players, game duration and port are integers without the `<>`

The server should be started in `127.0.0.1:<port>`

#### Client

To start a client,

Navigate to `haskell`

Run `cabal run`

This should run a client that connects to `127.0.0.1` in port `15000`

## References

### Dart

### Haskell

Server Boilerplate Code from [https://jaspervdj.be/websockets/example/server.html](https://jaspervdj.be/websockets/example/server.html)

Client Boilerplate Code from [https://github.com/haskell-miso/miso-websocket/blob/main/src/WebSocket.hs](https://github.com/haskell-miso/miso-websocket/blob/main/src/WebSocket.hs)
