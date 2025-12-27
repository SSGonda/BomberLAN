-- =-----------------------------------=
--          NECESSARY IMPORTS
-- =-----------------------------------=

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever, unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import Debug.Trace (trace)
import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           GHC.Generics
import           Miso.Lens
import           Miso.WebSocket
import           Miso.String (ToMisoString, MisoString)
import qualified Miso.String as MS
import Data.Time (UTCTime, getCurrentTime)
import System.Random (getStdRandom, randomR)

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL

-- Check whether to run a server or client

main :: IO ()
main = do
    args <- getArgs
    let mainToRun = case args of
            ["--host"] -> mainServer -- TODO ASK WHETHER THIS IS CORRECT
            _ -> mainClient
    mainToRun

-- =--------------------------------=
--           SERVER CODE
-- =--------------------------------=

-- THE FOLLOWING CODE IS DERIVED FROM THE EXAMPLE SECTION OF THE WEBSOCKETS DOCUMENTATION BY JASPERVDJ
-- https://jaspervdj.be/websockets/example/server.html

type Client = (Int, WS.Connection)

data ServerState = ServerState {
  clients :: [Client],
  gameState :: GameState
}

data Player = Player {
  id :: Int,
  x :: Float,
  y :: Float,
  maxBombs :: Int,
  currentBombs :: Int,
  bombRange :: Int,
  speed :: Float,
  isAlive :: Bool,
  activePowerups :: [Text]
} deriving (Generic, Show)

instance FromJSON Player
instance ToJSON Player

data Bomb = Bomb {
  player :: Int,
  x :: Int,
  y :: Int,
  timePlaced :: UTCTime,
  maxTime :: Int,
  radius :: Int
} deriving (Generic, Show)

instance FromJSON Bomb
instance ToJSON Bomb

data Powerup = Powerup {
  name :: Text,
  x :: Int,
  y :: Int
} deriving (Generic, Show)

instance FromJSON Powerup
instance ToJSON Powerup

data Explosion = Explosion {
  timePlaced :: UTCTime,
  x :: Int,
  y :: Int
} deriving (Generic, Show)

instance FromJSON Explosion
instance ToJSON Explosion

data GameState = GameState {
  maxPlayers :: Int,
  gameDuration :: Int,
  isGameStarted :: Bool,
  isGameOver :: Bool,
  gameStartTime :: UTCTime,
  winner :: Maybe Int,
  players :: [Player],
  bombs :: [Bomb],
  powerups :: [Powerup],
  explosions :: [Explosion],
  grid :: [[Int]]
} deriving (Generic, Show)

instance FromJSON GameState
instance ToJSON GameState

data ServerResponse = ServerResponse {
  tag :: Text,
  gameState :: Maybe GameState,
  message :: Maybe String
} deriving (Generic, Show)

instance FromJSON ServerResponse
instance ToJSON ServerResponse

initPlayerPositions :: [(Int, Float, Float)] -- id, x, y
initPlayerPositions = [(0, 1.0, 1.0), (1, 1.0, 13.0), (2, 11.0, 13.0), (3, 11.0, 1.0)]

initPlayer :: Int -> Float -> Float -> Player
initPlayer id x y = Player {
  id = id,
  x = x,
  y = y,
  maxBombs = 1,
  currentBombs = 0,
  bombRange = 1,
  speed = 1.0,
  isAlive = True,
  activePowerups = []
}

positionToPlayer :: (Int, Float, Float) -> Player
positionToPlayer (id, x, y) = initPlayer id x y

makePlayers :: Int -> [Player]
makePlayers numPlayers = map positionToPlayer (take numPlayers initPlayerPositions)

emptyGrid :: [[Int]]
emptyGrid = [[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]]

randomChanceEachRow :: [Int] -> IO [Int]
randomChanceEachRow = mapM randSoft
  where
    randSoft :: Int -> IO Int
    randSoft c = case c of
      0 -> do
        p <- getStdRandom (randomR (0, 99))
        if p < 40 then
          return 1
        else
          return 0
      x -> return x

initGrid :: IO [[Int]]
initGrid = mapM randomChanceEachRow emptyGrid

initGameState :: Int -> Int -> IO GameState
initGameState numPlayers gameDuration = do
  currentTime <- getCurrentTime
  grid <- initGrid
  return GameState {
    maxPlayers = numPlayers,
    gameDuration = gameDuration,
    isGameStarted = False,
    isGameOver = False,
    gameStartTime = currentTime, -- Dummy start time
    winner = Nothing,
    players = makePlayers numPlayers,
    bombs = [],
    powerups = [],
    explosions = [],
    grid = grid
  }

newServerState :: IO ServerState
newServerState = do
  initialGameState <- initGameState 2 120 -- TODO unhardcode this
  return ServerState {
    clients = [],
    gameState = initialGameState
  }

numClients :: ServerState -> Int
numClients s = length s.clients

clientExists :: Client -> ServerState -> Bool
clientExists client s = any ((== fst client) . fst) s.clients

addClient :: Client -> ServerState -> ServerState
addClient client s = s { clients = client : clients s }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = s { clients = filter ((/= fst client) . fst) s.clients }

broadcast :: BL.ByteString -> ServerState -> IO ()
broadcast message s = do
    print message
    forM_ s.clients $ \(_, conn) -> WS.sendTextData conn message

mainServer :: IO ()
mainServer = do
    initialState <- newServerState
    state <- newMVar initialState
    putStrLn "Server Running"
    WS.runServer "127.0.0.1" 15000 $ application state -- Port Hardcoded as per Phase 2 specs

application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO Text
    sstate <- readMVar state
    let cid = numClients sstate
    let client = (cid, conn)
    flip finally (disconnect client) $ do
        modifyMVar_ state $ \s -> do
            let s' = addClient client s
            print (show (fst client) <> " joined")
            broadcast (encode ServerResponse { tag = "StateUpdate", gameState = Just sstate.gameState, message = Nothing}) s'
            return s'
        talk conn state client
    where
      disconnect c = do
          -- Remove client and return new state
          s <- modifyMVar state $ \s ->
              let s' = removeClient c s in return (s', s')
          print (show (fst c) <> " disconnected")

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn :: IO Text
    readMVar state >>= broadcast (encode ServerResponse { tag = "UserMessage", gameState = Nothing, message = Just (show user <> " : " <> T.unpack msg) } )

-- =-----------------------------------=
--             CLIENT CODE
-- =-----------------------------------=

-- THE FOLLOWING CODE IS DERIVED FROM THE EXAMPLE OF THE WEBSOCKETS DOCUMENTATION BY MISO
-- https://github.com/haskell-miso/miso-websocket/blob/main/src/WebSocket.hs

-----------------------------------------------------------------------------
data Message
  = Message
  { dateString :: MisoString
  , message :: MisoString
  , origin :: Origin
  } deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
data Origin = CLIENT | SYSTEM | SERVER
  deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
instance ToMisoString Origin where
  toMisoString x = case x of
    CLIENT -> "CLIENT"
    SYSTEM -> "SYSTEM"
    SERVER -> "SERVER"
-----------------------------------------------------------------------------
data Action
  = OnOpen WebSocket
  | OnMessage MisoString
  | OnClosed Closed
  | OnError MisoString
  | Send
  | SendMessage MisoString
  | Update MisoString
  | Append Message
  | Connect
  | Disconnect
  | NoOp
  | CloseBox
  | Clear
-----------------------------------------------------------------------------
data Model = Model
  { _msg :: MisoString
  , _received :: [Message]
  , _websocket :: WebSocket
  , _connected :: Bool
  , _connections :: [WebSocket]
  , _clearInput :: Bool
  , _boxId :: Int
  } deriving Eq
-----------------------------------------------------------------------------
msg :: Lens Model MisoString
msg = lens _msg $ \r x -> r { _msg = x }
-----------------------------------------------------------------------------
received :: Lens Model [Message]
received = lens _received $ \r x -> r { _received = x }
-----------------------------------------------------------------------------
websocket :: Lens Model WebSocket
websocket = lens _websocket $ \r x -> r { _websocket = x }
-----------------------------------------------------------------------------
connected :: Lens Model Bool
connected = lens _connected $ \r x -> r { _connected = x }
-----------------------------------------------------------------------------
clearInput :: Lens Model Bool
clearInput = lens _clearInput $ \r x -> r { _clearInput = x }
-----------------------------------------------------------------------------
boxId :: Lens Model Int
boxId = lens _boxId $ \r x -> r { _boxId = x }
-----------------------------------------------------------------------------
emptyModel :: Int -> Model
emptyModel = Model mempty [] emptyWebSocket False [] True
-----------------------------------------------------------------------------
websocketComponent :: Int -> M.Component parent Model Action
websocketComponent box =
  (M.component (emptyModel box) updateModel viewModel)
    { M.events = M.defaultEvents <> M.keyboardEvents
    }
  where
    updateModel x = case x of
      Send -> do
        m <- use msg
        unless (MS.null m) $ do
          M.issue (SendMessage m)
          clearInput .= True
          msg .= ""
          M.io $ do
            date <- M.newDate
            dateString <- date & M.toLocaleString
            pure $ Append (Message dateString m CLIENT)
      SendMessage m -> do
        socket <- use websocket
        sendText socket m
      Connect ->
        connectText
          "ws:127.0.0.1:15000"
          OnOpen
          OnClosed
          OnMessage
          OnError
      OnOpen socket -> do
        websocket .= socket
        connected .= True
      OnClosed closed -> do
        connected .= False
        M.io $ do
          date <- M.newDate
          dateString <- date & M.toLocaleString
          M.consoleLog $ MS.ms (show closed)
          pure $ Append (Message dateString "Disconnected..." SYSTEM)
      OnMessage message ->
        M.io $ do
          date <- M.newDate
          dateString <- date & M.toLocaleString
          pure $ Append (Message dateString (MS.ms $ show ( decode (MS.fromMisoString message) :: Maybe ServerResponse)) SERVER)
      Append message ->
        received %= (message :)
      OnError errorMessage ->
        M.io_ (M.consoleError errorMessage)
      Update input -> do
        clearInput .= False
        msg .= input
      NoOp ->
        pure ()
      CloseBox ->
        M.broadcast box
      Disconnect ->
        close =<< use websocket
      Clear -> do
        clearInput .= True
        msg .= ""
        received .= []
-----------------------------------------------------------------------------
viewModel :: Model -> M.View Model Action
viewModel m =
  H.div_
  [ P.className "websocket-box" ]
  [ H.div_
    [ P.class_ "websocket-header" ]
    [ H.div_
      []
      [ H.span_
        [ P.classList_
          [ ("websocket-status", True)
          , ("status-disconnected", not (m ^. connected))
          , ("status-connected", m ^. connected)
          ]
        ]
        []
      , H.span_
        [ P.class_ "websocket-id"
        ]
        [ M.text $ "socket-" <> MS.ms (m ^. boxId) ]
      ]
    , H.button_
      [ P.aria_ "label" "Close"
      , P.class_ "btn-close"
      , H.onClick CloseBox
      ]
      [ "×" ]
    ]
    , H.div_
      [ P.class_ "websocket-controls" ]
      [ M.optionalAttrs
        H.button_
        [ P.class_ "btn btn-success connect-btn"
        , H.onClick Connect
        ]
        (m ^. connected)
        [ P.disabled_ ]
        [ "Connect" ]
      , M.optionalAttrs
        H.button_
        [ P.class_ "btn btn-danger disconnect-btn"
        , H.onClick Disconnect
        ]
        (not (m ^. connected))
        [ P.disabled_ ]
        ["Disconnect"]
      , M.optionalAttrs
        H.button_
        [ P.class_ "btn btn-primary"
        , H.onClick Clear
        ]
        (null (m ^. received))
        [ P.disabled_ ]
        ["Clear"]
      ]
    , H.div_
      [ P.class_ "websocket-input" ]
      [ H.input_ $
        [ P.placeholder_ "Type a message..."
        , P.class_ "input-field message-input"
        , H.onInput Update
        , H.onEnter NoOp Send
        , P.type_ "text"
        ] ++
        [ P.disabled_
        | not (m ^. connected)
        ] ++
        [ P.value_ ""
        | m ^. clearInput
        ]
      , M.optionalAttrs
        H.button_
        [ P.class_ "btn btn-primary send-btn"
        , H.onClick Send
        ]
        (not (m ^. connected))
        [ P.disabled_ ]
        [ "Send"
        ]
      ]
    , H.div_
      [ P.class_ "messages-list"
      ] $
      if null (m ^. received)
      then
        pure $ H.div_
          [ P.class_ "empty-state"
          ]
          [ "No messages yet"
          ]
      else messageHeader (m ^. received)
    ]
-----------------------------------------------------------------------------
messageHeader :: [Message] -> [ M.View model action ]
messageHeader messages = concat
  [
    [ H.div_
      [ P.class_ "message-header" ]
      [ H.span_ [P.class_ "message-origin"] [ M.text (MS.ms origin) ]
      , H.span_ [P.class_ "timestamp"] [ M.text dateString ]
      ]
    , H.div_ [P.class_ "message-content"] [ M.text message ]
    ]
  | Message dateString message origin <- messages
  ]
-----------------------------------------------------------------------------
mainClient :: IO ()
mainClient = M.run $ M.startApp (websocketComponent 0) -- 0 is the socket id



