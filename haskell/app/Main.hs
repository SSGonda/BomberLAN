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
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever, unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, threadDelay, forkIO)
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import GHC.Generics ( Generic )
import Miso.Lens ( (&), (%=), (.=), (^.), lens, use, Lens )
import Miso.WebSocket
    ( emptyWebSocket, close, connectText, sendText, Closed, WebSocket )
import           Miso.String (ToMisoString, MisoString)
import qualified Miso.String as MS
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime)
import System.Random (getStdRandom, randomR)
import Data.List (partition)

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
  deltaX :: Int,
  deltaY :: Int,
  maxBombs :: Int,
  currentBombs :: Int,
  bombRange :: Int,
  speed :: Float,
  isAlive :: Bool,
  activePowerups :: [Text]
} deriving (Eq, Generic, Show)

instance FromJSON Player
instance ToJSON Player

data Bomb = Bomb {
  player :: Int,
  x :: Int,
  y :: Int,
  timePlaced :: UTCTime,
  maxTime :: Int,
  radius :: Int
} deriving (Eq, Generic, Show)

instance FromJSON Bomb
instance ToJSON Bomb

data Powerup = Powerup {
  name :: Text,
  x :: Int,
  y :: Int
} deriving (Eq, Generic, Show)

instance FromJSON Powerup
instance ToJSON Powerup

data Explosion = Explosion {
  timePlaced :: UTCTime,
  x :: Int,
  y :: Int
} deriving (Eq, Generic, Show)

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
} deriving (Eq, Generic, Show)

instance FromJSON GameState
instance ToJSON GameState

data ServerResponse = ServerResponse {
  tag :: Text,
  gameState :: Maybe GameState,
  message :: Maybe String
} deriving (Generic, Show)

instance FromJSON ServerResponse
instance ToJSON ServerResponse

data ClientRequest = ClientRequest {
  tag :: Text,
  action :: Text
} deriving (Generic, Show)

instance FromJSON ClientRequest
instance ToJSON ClientRequest

initPlayerPositions :: [(Int, Float, Float)] -- id, x, y
initPlayerPositions = [(0, 1.0, 1.0), (1, 1.0, 13.0), (2, 11.0, 13.0), (3, 11.0, 1.0)]

initPlayer :: Int -> Float -> Float -> Player
initPlayer id x y = Player {
  id = id,
  x = x,
  y = y,
  deltaX = 0,
  deltaY = 0,
  maxBombs = 1,
  currentBombs = 0,
  bombRange = 1,
  speed = 0.1,
  isAlive = True,
  activePowerups = []
}

positionToPlayer :: (Int, Float, Float) -> Player
positionToPlayer (id, x, y) = initPlayer id x y

makePlayers :: Int -> [Player]
makePlayers numPlayers = map positionToPlayer (take numPlayers initPlayerPositions)

emptyGrid :: [[Int]]
emptyGrid = [[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2], -- Legend: 3 is a hardcoded spot that will be 0 after randomization
            [2,3,0,0,0,0,0,0,0,0,0,0,0,3,2],  -- 2 is a hard block
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],  -- 0 all have a chance to become 1, a soft block after randomizaiton
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,0,0,0,0,0,0,0,0,0,0,0,0,0,2],
            [2,0,2,0,2,0,2,0,2,0,2,0,2,0,2],
            [2,3,0,0,0,0,0,0,0,0,0,0,0,3,2],
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
      3 -> return 0
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

newServerState :: Int -> Int -> IO ServerState
newServerState maxPlayers duration = do
  initialGameState <- initGameState maxPlayers duration
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
    initialState <- newServerState 2 120 -- TODO unhardcode this
    state <- newMVar initialState
    putStrLn "Server Running"

    _ <- forkIO $ gameLoop state

    WS.runServer "127.0.0.1" 15000 $ application state -- TODO unhardcode this Port Hardcoded as per Phase 2 specs

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
            WS.sendTextData conn (encode ServerResponse { tag = "ClientJoin", gameState = Nothing, message = Just (show cid)})
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
    msg <- WS.receiveData conn
    print msg
    case (decode msg :: Maybe ClientRequest) of
      Just cr -> do
        modifyMVar_ state $ \s -> do
          gs <- parseClientRequest user cr s.gameState
          let s' = (s { gameState = gs }) :: ServerState

          broadcast (encode ServerResponse {
            tag = "StateUpdate",
            gameState = Just gs,
            message = Nothing
          }) s'

          return s'
      Nothing ->
        readMVar state >>= broadcast (encode ServerResponse { tag = "InvalidClientInput", gameState = Nothing, message = Just (show user <> " : " <> "gave invalid request") } )

gameLoop :: MVar ServerState -> IO ()
gameLoop state = forever $ do
  currentTime <- getCurrentTime
  modifyMVar_ state $ \s -> do
    let gs = s.gameState

    let gs'
          | not gs.isGameStarted && (numClients s >= gs.maxPlayers) = gs { isGameStarted = True, gameStartTime = currentTime } -- start the game
          | gs.isGameOver || not gs.isGameStarted = gs -- game not started or game over
          | otherwise = updateGameState currentTime gs -- update game

    let s' = (s { gameState = gs' }) :: ServerState

    unless (gs == gs') $ do
      broadcast (encode ServerResponse {
        tag = "StateUpdate",
        gameState = Just gs',
        message = Nothing
      }) s'

    return s'
  threadDelay 16667 -- 16.667 ms to microseconds, 1000 ms / 60 frames

initExplosion :: UTCTime -> Int -> Int -> Explosion
initExplosion currentTime x y = Explosion {
    timePlaced = currentTime,
    x = x,
    y = y
  }

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex 0 (x' : _) = Just x'
safeIndex index (_ : xs) = safeIndex (index - 1) xs

safeGridIndex :: Int -> Int -> [[a]] -> Maybe a
safeGridIndex i j grid = a
  where
    row = safeIndex i grid
    a = case row of
      Just r -> safeIndex j r
      Nothing -> Nothing

replace :: a -> Int -> [a] -> [a]
replace _ _ [] = []
replace a 0 (_ : xs) = a : xs
replace a index (x' : xs) = x' : replace a (index - 1) xs

gridReplace :: a -> Int -> Int -> [[a]] -> [[a]]
gridReplace a i j grid = grid'
  where
    row = case safeIndex i grid of
      Just r -> Just (replace a j r)
      Nothing -> Nothing
    grid' = case row of
      Just r -> replace r i grid
      Nothing -> grid

explosionPositions :: [(Int, Int)]
explosionPositions = [(0, 0), (0, 1), (1, 0), (0, -1), (-1, 0)]

explodeCoord :: (Int, Int) -> UTCTime -> GameState -> GameState
explodeCoord (x, y) currentTime gs = gs { grid = grid', bombs = bombs', powerups = powerups', explosions = explosions' }
  where
    categorizeBlocks :: [(Int, Int)] -> [[Int]] -> ([(Int, Int)], [(Int, Int)])
    categorizeBlocks [] _ = ([], [])
    categorizeBlocks coords grd = foldr categorizeBlock ([], []) coords
      where
        categorizeBlock :: (Int, Int) -> ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
        categorizeBlock (i, j) (explodables', softs') = case safeGridIndex i j grd of
          Just 0 -> ((i, j) : explodables', softs')
          Just 1 -> (explodables', (i, j) : softs')
          _ -> (explodables', softs')

    removeSoft :: [(Int, Int)] -> [[Int]] -> [[Int]]
    removeSoft [] grd = grd
    removeSoft ((i, j) : coords) grd = removeSoft coords (gridReplace 0 i j grd)

    placeExplosives :: [(Int, Int)] -> [Explosion] -> [Explosion]
    placeExplosives [] es = es
    placeExplosives ((i, j) : coords) es = Explosion { timePlaced = currentTime, x = i, y = j } : placeExplosives coords es

    detonateBombs :: [(Int, Int)] -> [Bomb] -> [Bomb]
    detonateBombs [] bs = bs
    detonateBombs _ [] = []
    detonateBombs (coord : coords) bs = detonateBombs coords (detonateBomb coord bs)
      where
        detonateBomb :: (Int, Int) -> [Bomb] -> [Bomb]
        detonateBomb _ [] = []
        detonateBomb (i, j) (b : bs')
          | b.x == i && b.y == j = b { maxTime = 0 } : bs' -- TODO HORRIBLE HACK TO MAKE A BOMB EXPLODE ON THE NEXT TICK, which is at least 16 ms
          | otherwise = b : detonateBomb (i, j) bs'

    removePowerups :: [(Int, Int)] -> [Powerup] -> [Powerup]
    removePowerups [] ps = ps
    removePowerups _ [] = []
    removePowerups ((i, j) : coords) ps = removePowerups coords (filter (\p -> p.x /= i && p.y /= j) ps)

    (explodables, softs) = categorizeBlocks (map (\(i, j) -> (i + x, j + y)) explosionPositions) gs.grid

    grid' = removeSoft softs gs.grid
    bombs' = detonateBombs explodables gs.bombs
    powerups' = removePowerups explodables gs.powerups
    explosions' = placeExplosives explodables gs.explosions
    -- TODO add handling for damaging players

explodeCoords :: [(Int, Int)] -> UTCTime -> GameState -> GameState
explodeCoords [] _ gs = gs
explodeCoords (coord : coords) currentTime gs = explodeCoords coords currentTime gs'
  where
    gs' = explodeCoord coord currentTime gs

updateExpiredExplosions :: UTCTime -> GameState -> GameState
updateExpiredExplosions currentTime gs = gs { explosions = filter (\e -> not (isExpired currentTime e.timePlaced 1)) gs.explosions }

updateExplosionAreas :: GameState -> GameState
updateExplosionAreas gs = gs { players = playersCheckExplosions gs.players}
  where
    playerCollidesWithExplosions :: Player -> Bool
    playerCollidesWithExplosions p = any (\e -> isCollideWithPlayer 1 p.x p.y e.x e.y) gs.explosions

    playersCheckExplosions :: [Player] -> [Player]
    playersCheckExplosions [] = []
    playersCheckExplosions (p : ps) = 
      if playerCollidesWithExplosions p then
        p { isAlive = False } : playersCheckExplosions ps
      else
        p : playersCheckExplosions ps

updateBombs :: UTCTime -> GameState -> GameState
updateBombs currentTime gs = gs''
  where
    (expired, active) = partition (\b -> isExpired currentTime b.timePlaced b.maxTime) gs.bombs

    removeBomb :: Bomb -> [Player] -> [Player]
    removeBomb _ [] = []
    removeBomb b (p : ps)
      | b.player == p.id = p { currentBombs = p.currentBombs - 1 } : ps
      | otherwise = p : removeBomb b ps

    removeBombs :: [Bomb] -> [Player] -> [Player]
    removeBombs [] ps = ps
    removeBombs _ [] = []
    removeBombs (b : bs) ps = removeBombs bs ps'
      where
        ps' = removeBomb b ps

    coordsToExplode = map (\b -> (b.x, b.y)) expired
    gs' = gs { bombs = active, players = removeBombs expired gs.players } -- remove bombs from gamestate and decrement player active bombs
    gs'' = explodeCoords coordsToExplode currentTime gs' -- add explosions, and do relevant explosion behaviour in gamestate

isExpired :: UTCTime -> UTCTime -> Int -> Bool
isExpired t1 t2 offset = abs (nominalDiffTimeToSeconds (diffUTCTime t1 t2)) >= fromIntegral offset

updateGameState :: UTCTime -> GameState -> GameState
updateGameState currentTime gs =
  gs
    & updatePlayerPositions
    & updateBombs currentTime
    & updateExpiredExplosions currentTime
    & updateExplosionAreas

initBomb :: Int -> Int -> Int -> Int -> Int -> IO Bomb
initBomb pid x y maxTime radius = do
  currentTime <- getCurrentTime
  return Bomb {
    player = pid,
    x = x,
    y = y,
    timePlaced = currentTime,
    maxTime = maxTime,
    radius = radius
  }

updatePlayerBomb :: Int -> Int -> GameState -> IO GameState
updatePlayerBomb pid delta gs = do
  let p = safeIndex pid gs.players
  case p of
    Just pl -> if pl.currentBombs < pl.maxBombs && pl.isAlive then do
      bomb <- initBomb pl.id (round pl.x) (round pl.y) 3 pl.bombRange
      let players' = replace ((pl { currentBombs = pl.currentBombs + delta }) :: Player) pid gs.players
      return gs { players = players', bombs = bomb : gs.bombs }
      else return gs
    Nothing -> return gs

updatePlayerDelta :: Int -> Int -> Int -> GameState -> GameState
updatePlayerDelta pid dx dy gs = gs { players = players' }
  where
    players' = case safeIndex pid gs.players of
      Just p -> replace ((p { deltaX = dx, deltaY = dy }) :: Player) pid gs.players
      Nothing -> gs.players

square :: Float -> Float
square a = a * a

getCollideable :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
getCollideable [] _ = []
getCollideable ((i, j) : coords) grd = case safeGridIndex i j grd of
  Just 1 -> (i, j) : getCollideable coords grd
  Just 2 -> (i, j) : getCollideable coords grd
  _ -> getCollideable coords grd

isCollideWithPlayer :: Float -> Float -> Float -> Int -> Int -> Bool
isCollideWithPlayer threshold px py ex ey = distance px py ex ey < threshold

distance :: Float -> Float -> Int -> Int -> Float
distance x1 y1 x2 y2 = sqrt (square (x1 - fromIntegral x2) + square (y1 - fromIntegral y2))

updatePlayerPosition :: GameState -> Player -> Player
updatePlayerPosition gs p = p { x = x'', y = y'' }
  where
    x' = p.x + fromIntegral p.deltaX * p.speed
    y' = p.y + fromIntegral p.deltaY * p.speed

    positionsToCheck = map (\(i, j) -> (i + round x', j + round y')) explosionPositions
    collideableGridPositions = getCollideable positionsToCheck gs.grid

    bombDistances = map (\b -> (distance p.x p.y b.x b.y, distance x' y' b.x b.y)) gs.bombs

    isGoingToBomb = any (\(before, after) -> before >= 0.5 && after < 0.5) bombDistances

    (x'', y'') = if any (uncurry (isCollideWithPlayer 0.8 x' y')) collideableGridPositions || isGoingToBomb || not p.isAlive then (p.x, p.y) else (x', y')

updatePlayerPositions :: GameState -> GameState
updatePlayerPositions gs = gs { players = map (updatePlayerPosition gs) gs.players }

parseClientRequest :: Int -> ClientRequest -> GameState -> IO GameState
parseClientRequest pid cr gs
  | gs.isGameOver || not gs.isGameStarted = return gs
  | cr.action == "up" = return (updatePlayerDelta pid (-1) 0 gs)
  | cr.action == "down" = return (updatePlayerDelta pid 1 0 gs)
  | cr.action == "left" = return (updatePlayerDelta pid 0 (-1) gs)
  | cr.action == "right" = return (updatePlayerDelta pid 0 1 gs)
  | cr.action == "stop" = return (updatePlayerDelta pid 0 0 gs)
  | cr.action == "bomb" = updatePlayerBomb pid 1 gs
  | otherwise = return gs

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
--------------
jsonRequest :: Text -> MisoString
jsonRequest a = MS.ms (encode ClientRequest {
  tag = "ClientUpdate",
  action = a
})
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
      [ P.class_ "websocket-input"]
      [ H.button_ [H.onClick (SendMessage (jsonRequest "up"))] [M.text "Up"]
        , H.button_ [H.onClick (SendMessage (jsonRequest "down"))] [M.text "Down"]
        , H.button_ [H.onClick (SendMessage (jsonRequest "left"))] [M.text "Left"]
        , H.button_ [H.onClick (SendMessage (jsonRequest "right"))] [M.text "Right"]
        , H.button_ [H.onClick (SendMessage (jsonRequest "stop"))] [M.text "Stop"]
        , H.button_ [H.onClick (SendMessage (jsonRequest "bomb"))] [M.text "Bomb"]
       ]
    -- , H.div_
    --   [ P.class_ "websocket-input" ]
    --   [ H.input_ $
    --     [ P.placeholder_ "Type a message..."
    --     , P.class_ "input-field message-input"
    --     , H.onInput Update
    --     , H.onEnter NoOp Send
    --     , P.type_ "text"
    --     ] ++
    --     [ P.disabled_
    --     | not (m ^. connected)
    --     ] ++
    --     [ P.value_ ""
    --     | m ^. clearInput
    --     ]
    --   , M.optionalAttrs
    --     H.button_
    --     [ P.class_ "btn btn-primary send-btn"
    --     , H.onClick Send
    --     ]
    --     (not (m ^. connected))
    --     [ P.disabled_ ]
    --     [ "Send"
    --     ]
    --   ]

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
mainClient = M.run $ M.startApp (websocketComponent 0)