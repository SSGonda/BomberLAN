-- =-----------------------------------=
--          NECESSARY IMPORTS
-- =-----------------------------------=

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Text (Text)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Control.Exception (finally, catch, throwIO, SomeException)
import Control.Monad (forM_, forever, unless, foldM, when)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, threadDelay, forkIO)
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import qualified Miso as M
import Miso.Subscription.Keyboard (keyboardSub)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.CSS as CSS
import qualified Miso.CSS.Color as Col
import GHC.Generics ( Generic )
import Miso.Lens ( (&), (%=), (.=), (^.), lens, use, Lens )
import Miso.WebSocket
    ( emptyWebSocket, close, connectText, sendText, Closed, WebSocket )
import           Miso.String (ToMisoString, MisoString)
import qualified Miso.String as MS
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime, addUTCTime)
import System.Random (getStdRandom, randomR)
import Data.List (partition)

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Ix (Ix(range))
import Data.Functor ( (<&>) )

import qualified Miso.Canvas as Canvas
import qualified Language.Javascript.JSaddle as JSaddle
import Miso.Canvas (TextAlignType (TextAlignCenter))


-- Check whether to run a server or client

mainNothing :: IO ()
mainNothing = do
  putStrLn "Invalid Arguments"

main :: IO ()
main = do
    args <- getArgs
    let mainToRun = case args of
            [numPlayers, gameDura, "--host", port] -> do
              let numPlayersInt = read numPlayers :: Int
              let gameDuraInt = read gameDura :: Int
              if numPlayersInt < 2 || numPlayersInt > 4 || gameDuraInt < 30 || gameDuraInt > 600
                then mainNothing
                else mainServer numPlayersInt gameDuraInt (read port :: Int)
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
  timeRemaining :: Int,
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
  action :: Text,
  player :: Int
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
    timeRemaining = gameDuration,
    winner = Nothing,
    players = makePlayers numPlayers,
    bombs = [],
    powerups = [],
    explosions = [],
    grid = grid
  }

getTimeRemaining :: UTCTime -> GameState -> Int
getTimeRemaining currentTime gs
  | not gs.isGameStarted = gs.gameDuration
  | gs.isGameOver = 0
  | otherwise = max 0 (floor (nominalDiffTimeToSeconds (diffUTCTime (addUTCTime (fromIntegral gs.gameDuration) gs.gameStartTime) currentTime)))

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

mainServer :: Int -> Int -> Int -> IO ()
mainServer numPlayers gameDura port = do
    initialState <- newServerState numPlayers gameDura
    state <- newMVar initialState
    putStrLn "Server Running"

    _ <- forkIO $ gameLoop state

    WS.runServer "127.0.0.1" port $ application state

application :: MVar ServerState -> WS.PendingConnection -> IO ()
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

    gs' <-
      if not gs.isGameStarted && (numClients s >= gs.maxPlayers) then return gs { isGameStarted = True, gameStartTime = currentTime, timeRemaining = gs.gameDuration } -- start the game
      else if gs.isGameOver || not gs.isGameStarted then return gs -- game not started or game over
      else updateGameState currentTime gs -- update game

    let s' = (s { gameState = gs' }) :: ServerState

    unless (gs == gs') $ do
      broadcast (encode ServerResponse {
        tag = "StateUpdate",
        gameState = Just gs',
        message = Nothing
      }) s'

    return s'
  threadDelay 16667 -- 16.667 ms to microseconds, 1000 ms / 60 frames

findWinner :: [Player] -> Maybe Int
findWinner [] = Nothing
findWinner ps = winner
  where
    alives = filter (\p -> p.isAlive) ps
    winner = case length alives of
      1 -> Just (head alives).id
      _ -> Nothing

updateCheckGameOver :: UTCTime -> GameState -> GameState
updateCheckGameOver currentTime gs
  | isExpired currentTime gs.gameStartTime gs.gameDuration = gs { isGameOver = True, winner = winr, timeRemaining = 0 }
  | otherwise = gs { isGameOver = gmover, winner = winr, timeRemaining = getTimeRemaining currentTime gs }
  where
    winr = findWinner gs.players
    gmover = case winr of
      Just _ -> True
      Nothing -> False

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

explosionPositions :: Int -> [(Int, Int)]
explosionPositions 0 = [(0, 0)]
explosionPositions r = [(0, 0)]
  <> range ((0, 1), (0, r))
  <> range ((1, 0), (r, 0))
  <> range ((-r, 0), (-1, 0))
  <> range ((0, -r), (0, -1))

spawnPowerup :: Text -> Int -> Int -> [Powerup] -> [Powerup]
spawnPowerup name x y ps = Powerup { name = name, x = x, y = y } : ps

spawnPowerupWithProbability :: [Powerup] -> Int -> Int -> Int -> IO [Powerup]
spawnPowerupWithProbability ps p x y = do
  r <- getStdRandom (randomR (0, 99))
  if r < p then do
    r' <- getStdRandom (randomR (0, 2))
    if r' == 0 then
      return (spawnPowerup "fireup" x y ps)
    else if r' == 1 then
      return (spawnPowerup "bombup" x y ps)
    else if r' == 2 then
      return (spawnPowerup "speedup" x y ps)
    else
      return ps
  else
    return ps

explodeCoord :: Int -> (Int, Int) -> UTCTime -> GameState -> IO GameState
explodeCoord r (x, y) currentTime gs = do
  powerups'' <- foldM (\p (i, j) -> spawnPowerupWithProbability p 10 i j) powerups' softs
  return gs { grid = grid', bombs = bombs', powerups = powerups'', explosions = explosions' }
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
    removePowerups ((i, j) : coords) ps = removePowerups coords (filter (\p -> p.x /= i || p.y /= j) ps)

    (explodables, softs) = categorizeBlocks (map (\(i, j) -> (i + x, j + y)) (explosionPositions r)) gs.grid

    grid' = removeSoft softs gs.grid
    bombs' = detonateBombs explodables gs.bombs
    powerups' = removePowerups explodables gs.powerups
    explosions' = placeExplosives explodables gs.explosions
    -- TODO add handling for damaging players

explodeCoords :: [(Int, Int, Int)] -> UTCTime -> GameState -> IO GameState
explodeCoords [] _ gs = return gs
explodeCoords ((x, y, r) : coords) currentTime gs = do
  gs' <- explodeCoord r (x, y) currentTime gs
  explodeCoords coords currentTime gs'

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

updatePowerupAreas :: GameState -> GameState
updatePowerupAreas gs = gs { players = players', powerups = powerups' }
  where
    applyPowerupForPlayer :: Powerup -> Player -> Player
    applyPowerupForPlayer Powerup { name = "fireup" } p = p { bombRange = p.bombRange + 1 }
    applyPowerupForPlayer Powerup { name = "bombup" } p = p { maxBombs = p.maxBombs + 1 }
    applyPowerupForPlayer Powerup { name = "speedup" } p = p { speed = p.speed * 1.3 }
    applyPowerupForPlayer _ p = p

    checkPowerupsForPlayer :: [Powerup] -> Player -> ([Powerup], Player)
    checkPowerupsForPlayer [] p = ([], p)
    checkPowerupsForPlayer (pup : pups) p
      | isCollideWithPlayer 1 p.x p.y pup.x pup.y = (pups, applyPowerupForPlayer pup p)
      | otherwise =  (pup : pups', p')
      where
        (pups', p') = checkPowerupsForPlayer pups p

    checkPlayersForPowerups :: [Powerup] -> [Player] -> ([Powerup], [Player])
    checkPlayersForPowerups pups [] = (pups, [])
    checkPlayersForPowerups pups (p : ps) = (pups'', p' : ps')
      where
        (pups', p') = checkPowerupsForPlayer pups p
        (pups'', ps') = checkPlayersForPowerups pups' ps
    
    (powerups', players') = checkPlayersForPowerups gs.powerups gs.players

updateBombs :: UTCTime -> GameState -> IO GameState
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

    coordsToExplode = map (\b -> (b.x, b.y, b.radius)) expired
    gs' = gs { bombs = active, players = removeBombs expired gs.players } -- remove bombs from gamestate and decrement player active bombs
    gs'' = explodeCoords coordsToExplode currentTime gs' -- add explosions, and do relevant explosion behaviour in gamestate

isExpired :: UTCTime -> UTCTime -> Int -> Bool
isExpired t1 t2 offset = nominalDiffTimeToSeconds (diffUTCTime t1 t2) >= fromIntegral offset

updateGameState :: UTCTime -> GameState -> IO GameState
updateGameState currentTime gs =
  gs
    & updatePlayerPositions
    & updateBombs currentTime 
    <&> updateExpiredExplosions currentTime 
    <&> updateExplosionAreas
    <&> updatePowerupAreas
    <&> updateCheckGameOver currentTime

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

    positionsToCheck = map (\(i, j) -> (i + round x', j + round y')) (explosionPositions 1)
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
  | KeyboardEvent IntSet
  | SendMessage MisoString
  | Update (Maybe GameState)
  | ChangeStatus MisoString
  | Connect
  | Disconnect
  | CloseBox
  | NoOp
-----------------------------------------------------------------------------
data Model = Model
  { _msg :: MisoString
  , _websocket :: WebSocket
  , _connected :: Bool
  , _connections :: [WebSocket]
  , _currentGameState :: Maybe GameState
  , _prevGameState :: Maybe GameState
  , _lastArrowDir :: (Int, Int)
  , _status :: Maybe MisoString
  , _heldKeys :: IntSet
  , _boxId :: Int
  } deriving Eq
-----------------------------------------------------------------------------
data CanvasState = CanvasState
  { floorImg :: M.Image
  , wallSoftImg :: M.Image
  , wallHardImg :: M.Image
  , bombImg :: M.Image
  , explosionImg :: M.Image
  , player1Img :: M.Image
  , player2Img :: M.Image
  , player3Img :: M.Image
  , player4Img :: M.Image
  , powerUpBombImg :: M.Image
  , powerUpFireImg :: M.Image
  , powerUpSpeedImg :: M.Image
  }

instance JSaddle.FromJSVal CanvasState where
  fromJSVal v = do
    ((floor, wallS, wallH, bomb, explosion, p1, p2), 
     (p3, p4, pupBomb, pupFire, pupSpeed)) <- JSaddle.fromJSValUnchecked v
    pure $ Just $ CanvasState floor wallS wallH bomb explosion p1 p2 p3 p4 pupBomb pupFire pupSpeed

instance JSaddle.ToJSVal CanvasState where
  toJSVal st = JSaddle.toJSVal 
    ((st.floorImg, st.wallSoftImg, st.wallHardImg, st.bombImg, st.explosionImg, 
      st.player1Img, st.player2Img),
     (st.player3Img, st.player4Img, st.powerUpBombImg, st.powerUpFireImg, 
      st.powerUpSpeedImg))
-----------------------------------------------------------------------------
initCanvas :: M.DOMRef -> Canvas.Canvas CanvasState 
initCanvas _ = JSaddle.liftJSM $ do
  floor <- M.newImage "../assets/images/floor.png"
  wallSoft <- M.newImage "../assets/images/wall_soft.png"
  wallHard <- M.newImage "../assets/images/wall_hard.png"
  bomb <- M.newImage "../assets/images/bomb.png"
  explosion <- M.newImage "../assets/images/explosion.png"
  player1 <- M.newImage "../assets/images/player1.png"
  player2 <- M.newImage "../assets/images/player2.png"
  player3 <- M.newImage "../assets/images/player3.png"
  player4 <- M.newImage "../assets/images/player4.png"
  powerUpBomb <- M.newImage "../assets/images/powerup_bomb.png"
  powerUpFire <- M.newImage "../assets/images/powerup_fire.png"
  powerUpSpeed <- M.newImage "../assets/images/powerup_speed.png"
  pure $ CanvasState {
    floorImg = floor,
    wallSoftImg = wallSoft,
    wallHardImg = wallHard,
    bombImg = bomb,
    explosionImg = explosion,
    player1Img = player1,
    player2Img = player2,
    player3Img = player3,
    player4Img = player4,
    powerUpBombImg = powerUpBomb,
    powerUpFireImg = powerUpFire,
    powerUpSpeedImg = powerUpSpeed
  }
-----------------------------------------------------------------------------
msg :: Lens Model MisoString
msg = lens _msg $ \r x -> r { _msg = x }
-----------------------------------------------------------------------------
websocket :: Lens Model WebSocket
websocket = lens _websocket $ \r x -> r { _websocket = x }
-----------------------------------------------------------------------------
connected :: Lens Model Bool
connected = lens _connected $ \r x -> r { _connected = x }
-----------------------------------------------------------------------------
boxId :: Lens Model Int
boxId = lens _boxId $ \r x -> r { _boxId = x }
-----------------------------------------------------------------------------
lastArrowDir :: Lens Model (Int, Int)
lastArrowDir = lens _lastArrowDir $ \r x -> r { _lastArrowDir = x }
-----------------------------------------------------------------------------
currentGameState :: Lens Model (Maybe GameState)
currentGameState = lens _currentGameState $ \r x -> r { _currentGameState = x }
-----------------------------------------------------------------------------
prevGameState :: Lens Model (Maybe GameState)
prevGameState = lens _prevGameState $ \r x -> r { _prevGameState = x }
-----------------------------------------------------------------------------
status :: Lens Model (Maybe MisoString)
status = lens _status $ \r x -> r { _status = x }
-----------------------------------------------------------------------------
heldKeys :: Lens Model IntSet
heldKeys = lens _heldKeys $ \r x -> r { _heldKeys = x }
-----------------------------------------------------------------------------
emptyModel :: Int -> Model
emptyModel = Model mempty emptyWebSocket False [] Nothing Nothing (0, 0) Nothing mempty
-----------------------------------------------------------------------------
websocketComponent :: Int -> M.Component parent Model Action
websocketComponent box =
  (M.component (emptyModel box) updateModel viewModel)
    { M.events = M.defaultEvents <> M.keyboardEvents
    , M.subs = [ keyboardSub KeyboardEvent ]
    }
  where
    updateModel x = case x of
      SendMessage m -> do
        socket <- use websocket
        sendText socket m
      Connect -> do
        currentGameState .= Nothing
        status .= Just "Connecting..."
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
          pure $ ChangeStatus "Disconnected..."
      OnMessage message -> do
        case decode (MS.fromMisoString message) :: Maybe ServerResponse of
          Just resp -> do
            case resp.tag of
              "StateUpdate" -> do
                case resp.gameState of
                  Just gs -> do 
                    M.issue (Update (Just gs))
                    status .= Nothing
                  Nothing -> pure ()   
              "ClientJoin" -> do
                case resp.message of
                  Just cid -> do
                    let clientId = read cid :: Int
                    boxId .= clientId
                    status .= Just ("Connected as Player " <> MS.ms (show (clientId + 1)))
                  Nothing -> pure ()       
              _ -> pure ()
          Nothing -> status .= Just "Could not parse message"
      Update gameState -> do
        oldGameState <- use prevGameState
        playerId <- use boxId
        case gameState of
          Just gs -> do
            -- game over sound
            when gs.isGameOver $ do
              M.io $ do
                let sound = case gs.winner of
                              Just w | w == playerId -> "win"
                              Just _ -> "lose"
                              Nothing -> "draw"
                cons <-  JSaddle.jsg ("Audio" :: String)
                audio <-JSaddle.new cons ["../assets/audio/" <> sound <> ".mp3" :: String]
                _ <- audio JSaddle.# ("play" :: String) $ ()
                pure $ NoOp 
            case oldGameState of
              Just oldGs -> do
                -- player death sound
                let oldAlive = (oldGs.players !! playerId).isAlive
                    newAlive = (gs.players !! playerId).isAlive
                when (oldAlive && not newAlive) $ do
                  M.io $ do
                    cons <-  JSaddle.jsg ("Audio" :: String)
                    audio <-JSaddle.new cons ["../assets/audio/death.mp3" :: String]
                    _ <- audio JSaddle.# ("play" :: String) $ ()
                    pure $ NoOp
              
                -- bomb explosion sound
                let oldBombs = length oldGs.bombs
                    newBombs = length gs.bombs
                when (newBombs < oldBombs) $ do
                  M.io $ do
                    cons <-  JSaddle.jsg ("Audio" :: String)
                    audio <-JSaddle.new cons ["../assets/audio/explosion.mp3" :: String]
                    _ <- audio JSaddle.# ("play" :: String) $ ()
                    pure $ NoOp

                -- get powerup sound
                let oldPowerups = length (oldGs.players !! playerId).activePowerups
                    newPowerups = length (gs.players !! playerId).activePowerups
                when (newPowerups > oldPowerups) $ do
                  M.io $ do
                    cons <-  JSaddle.jsg ("Audio" :: String)
                    audio <-JSaddle.new cons ["../assets/audio/powerup.mp3" :: String]
                    _ <- audio JSaddle.# ("play" :: String) $ ()
                    pure $ NoOp
              Nothing -> pure ()
          Nothing -> pure ()
          -- update game state
        old <- use currentGameState
        prevGameState .= old
        currentGameState .= gameState
      OnError errorMessage -> do
        M.io_ (M.consoleError errorMessage)
        status .= Just ("Error: " <> errorMessage)
      KeyboardEvent keys -> do
        isConnected <- use connected
        clientNumber <- use boxId
        oldKeys <- use heldKeys
        let dirs = IntSet.intersection keys (IntSet.fromList [37,38,39,40])
            safeHead [] = -1
            safeHead (x':_) = x' 
            newKeys = IntSet.difference dirs (IntSet.intersection oldKeys (IntSet.fromList [37,38,39,40]))
            activeKey = if not (IntSet.null newKeys)
                        then safeHead (IntSet.elems newKeys)
                        else safeHead (IntSet.elems dirs)
            action = case activeKey of
                      37 -> "left"
                      38 -> "up"
                      39 -> "right"
                      40 -> "down"
                      _ -> "stop"
        prevDir <- use lastArrowDir
        heldKeys .= keys
        let newDir = case action of
                      "left"  -> (0, -1)
                      "right" -> (0, 1)
                      "up"    -> (-1, 0)
                      "down"  -> (1, 0)
                      _       -> (0, 0)
        when (isConnected && newDir /= prevDir) $ do
          lastArrowDir .= newDir
          M.issue (SendMessage (jsonRequest action clientNumber))
        let wasBombPressed = IntSet.member 32 oldKeys
            isBombPressed = IntSet.member 32 keys
        when (isConnected && isBombPressed && not wasBombPressed) $ do
          M.issue (SendMessage (jsonRequest "bomb" clientNumber))
        unless isConnected $
          M.io_ $ M.consoleLog "Not connected, ignoring input"
      CloseBox ->
        M.broadcast box
      Disconnect -> do
        currentGameState .= Nothing
        status .= Just "Disconnecting..."
        close =<< use websocket
      NoOp -> pure ()
--------------
jsonRequest :: Text -> Int -> MisoString
jsonRequest a p = MS.ms (encode ClientRequest {
  tag = "ClientUpdate",
  action = a,
  player = p
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
      ]
    , case m ^. status of
      Just msg ->
        H.div_
        [ P.class_ "status-message"
        , CSS.style_ 
          [ CSS.margin "10px"
          , CSS.padding "10px"
          , CSS.textAlign "center"
          ]
        ]
        [ M.text msg ]
      Nothing -> H.div_ [] []
    , Canvas.canvas
        [ P.width_ (M.ms 600)
        , P.height_ (M.ms 560)
        ]
        initCanvas
        (renderCanvas m)
  ]
-----------------------------------------------------------------------------
leadingZeroes :: Int -> String
leadingZeroes n
  | n < 10 = "0" <> show n
  | otherwise = show n

renderCanvas :: Model -> CanvasState -> Canvas.Canvas ()
renderCanvas model canvasState = do
  -- clear
  Canvas.fillStyle (Canvas.ColorArg (Col.RGB 255 255 255))
  Canvas.fillRect (0, 0, 600, 560)

  let gsMaybe = model ^. currentGameState
  case gsMaybe of
    Nothing -> return ()
    Just gs -> do
      -- print time remaining
      Canvas.fillStyle (Canvas.ColorArg (Col.RGB 0 0 0))
      Canvas.font "20px Arial"
      Canvas.fillText (M.ms ("Time Remaining: " <> leadingZeroes (gs.timeRemaining `div` 60) <> ":" <> leadingZeroes (gs.timeRemaining `mod` 60)), 10, 20)
      -- draw floor and walls
      mapM_ (drawGridRow canvasState gs) [0..12]
      -- draw players
      mapM_ (drawPlayer canvasState) (filter (\player -> player.isAlive) gs.players)
      -- draw bombs
      mapM_ (drawBomb canvasState) gs.bombs
      -- draw powerups
      mapM_ (drawPowerup canvasState) gs.powerups
      -- draw explosions
      mapM_ (drawExplosion canvasState) gs.explosions

      when gs.isGameOver $ do
        -- print game over
        Canvas.fillStyle (Canvas.ColorArg (Col.RGB 255 255 255))
        Canvas.fillRect (100, 200, 400, 150)
        Canvas.fillStyle (Canvas.ColorArg (Col.RGB 0 0 0))
        Canvas.font "40px Arial"
        Canvas.fillText (M.ms "Game Over", 200, 250)
        case gs.winner of
          Just wid -> Canvas.fillText (M.ms ("Player " <> show (wid + 1) <> " Wins!"), 150, 300)
          Nothing -> Canvas.fillText (M.ms "It's a Draw!", 200, 300)
      

drawGridRow :: CanvasState -> GameState -> Int -> Canvas.Canvas ()
drawGridRow canvasState gs row = mapM_ drawCell [0..14]
  where
    drawCell col = do
      let cellValue = (gs.grid !! row) !! col
      let x = col * 40
      let y = row * 40 + 40
      case cellValue of
        0 -> Canvas.drawImage' (canvasState.floorImg, fromIntegral x, fromIntegral y, 40, 40)
        1 -> Canvas.drawImage' (canvasState.wallSoftImg, fromIntegral x, fromIntegral y, 40, 40)
        2 -> Canvas.drawImage' (canvasState.wallHardImg, fromIntegral x, fromIntegral y, 40, 40)
        _ -> return ()

drawPlayer :: CanvasState -> Player -> Canvas.Canvas ()
drawPlayer canvasState player = do
  let x = round (player.y * 40)
  let y = round (player.x * 40) + 40
  let playerImg = case player.id of
                    0 -> canvasState.player1Img
                    1 -> canvasState.player2Img
                    2 -> canvasState.player3Img
                    3 -> canvasState.player4Img
                    _ -> canvasState.player1Img
  -- draw player
  Canvas.drawImage' (playerImg, fromIntegral x, fromIntegral y, 40, 40)
  -- draw player label
  Canvas.fillStyle (Canvas.ColorArg (Col.RGB 255 255 255))
  Canvas.fillRect (fromIntegral x, fromIntegral y - 20, 45, 20)
  Canvas.fillStyle (Canvas.ColorArg (Col.RGB 0 0 0))
  Canvas.font "12px Arial"
  Canvas.fillText (M.ms ("Player " <> show (player.id + 1)), fromIntegral x, fromIntegral y - 10)

drawBomb :: CanvasState -> Bomb -> Canvas.Canvas ()
drawBomb canvasState bomb = do
  let x = bomb.y * 40
  let y = bomb.x * 40 + 40
  Canvas.drawImage' (canvasState.bombImg, fromIntegral x, fromIntegral y, 40, 40)

drawPowerup :: CanvasState -> Powerup -> Canvas.Canvas ()
drawPowerup canvasState powerup = do
  let x = powerup.y * 40
  let y = powerup.x * 40 + 40
  let powerupImg = case powerup.name of
                     "bombup" -> canvasState.powerUpBombImg
                     "fireup" -> canvasState.powerUpFireImg
                     "speedup" -> canvasState.powerUpSpeedImg
                     _ -> canvasState.powerUpBombImg
  Canvas.drawImage' (powerupImg, fromIntegral x, fromIntegral y, 40, 40)

drawExplosion :: CanvasState -> Explosion -> Canvas.Canvas ()
drawExplosion canvasState explosion = do
  let x = explosion.y * 40
  let y = explosion.x * 40 + 40
  Canvas.drawImage' (canvasState.explosionImg, fromIntegral x, fromIntegral y, 40, 40)
-----------------------------------------------------------------------------
mainClient :: IO ()
mainClient = M.run $ M.startApp (websocketComponent 0) -- 0 is the socket id