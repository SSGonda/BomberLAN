-- =-----------------------------------=
--          NECESSARY IMPORTS
-- =-----------------------------------=

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever, unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import System.Environment (getArgs)


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


type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

mainServer :: IO ()
mainServer = do
    state <- newMVar newServerState
    print "hello world"
    WS.runServer "127.0.0.1" 15000 $ application state -- Port Hardcoded as per Phase 2 specs

application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) ->
                WS.sendTextData conn ("Wrong announcement" :: Text)
            | any ($ fst client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn ("Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)
            | clientExists client clients ->
                WS.sendTextData conn ("User already exists" :: Text)
            | otherwise -> flip finally disconnect $ do
               modifyMVar_ state $ \s -> do
                   let s' = addClient client s
                   WS.sendTextData conn $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map fst s)
                   broadcast (fst client `mappend` " joined") s'
                   return s'
               talk conn state client
          where
            prefix     = "Hi! I am "
            client     = (T.drop (T.length prefix) msg, conn)
            disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                    let s' = removeClient client s in return (s', s')
                broadcast (fst client `mappend` " disconnected") s

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)

-- =-----------------------------------=
--             CLIENT CODE
-- =-----------------------------------=

-- THE FOLLOWING CODE IS DERIVED FROM THE EXAMPLE SECTION OF THE WEBSOCKETS DOCUMENTATION BY JASPERVDJ
-- https://github.com/jaspervdj/websockets/blob/master/example/client.hs

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
mainClient :: IO ()
mainClient = withSocketsDo $ WS.runClient "127.0.0.1" 15000 "/" app

-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoFieldSelectors #-}
 
-- module Main where
 
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import Debug.Trace (trace)
-- import qualified Miso as M
-- import qualified Miso.Html as H
-- import qualified Miso.Html.Property as P
 
-- main :: IO ()
-- main = M.run $ M.startApp app
--  where
--   app = M.component initModel update view
 
-- data FetchedData a
--   = FetchNotStarted
--   | Fetching
--   | Fetched a
--   deriving (Show, Eq)
 
-- type CounterId = Int
 
-- data Counter = Counter
--   { count :: Int
--   , counterId :: CounterId
--   }
--   deriving (Show, Eq)
 
-- data Model = Model
--   { nextId :: Int
--   , counters :: Map CounterId Counter
--   }
--   deriving (Show, Eq)
 
-- data Msg
--   = MsgAddNewCounter
--   | MsgIncrement CounterId
--   | MsgDecrement CounterId
--   | MsgReset CounterId
--   | MsgDelete CounterId
--   deriving (Show, Eq)
 
-- initModel :: Model
-- initModel =
--   Model
--     { nextId = 0
--     , counters = Map.empty
--     }
 
-- update :: Msg -> M.Transition Model Msg
-- update MsgAddNewCounter = do
--   model <- M.get
--   M.put
--     model
--       { nextId = model.nextId + 1
--       , counters =
--           Map.insert
--             model.nextId
--             ( Counter
--                 { count = 0
--                 , counterId = model.nextId
--                 }
--             )
--             model.counters
--       }
-- --
-- update (MsgIncrement counterId) = do
--   model <- M.get
--   M.put
--     model
--       { counters =
--           Map.adjust
--             ( \counter ->
--                 counter
--                   { count = counter.count + 1
--                   }
--             )
--             counterId
--             model.counters
--       }
-- --
-- update (MsgDecrement counterId) = do
--   model <- M.get
--   M.put
--     model
--       { counters =
--           Map.adjust
--             ( \counter ->
--                 counter
--                   { count = counter.count - 1
--                   }
--             )
--             counterId
--             model.counters
--       }
-- --
-- update (MsgReset counterId) = do
--   model <- M.get
--   M.put
--     model
--       { counters =
--           Map.adjust
--             ( \counter ->
--                 counter
--                   { count = 0
--                   }
--             )
--             counterId
--             model.counters
--       }
-- --
-- update (MsgDelete counterId) = do
--   model <- M.get
--   M.put
--     model
--       { counters =
--           Map.delete
--             counterId
--             model.counters
--       }
 
-- view :: Model -> M.View Model Msg
-- view model =
--   H.div_
--     []
--     [ H.button_ [H.onClick MsgAddNewCounter] [M.text "New counter"]
--     , H.br_ []
--     , viewCounters (snd <$> Map.toList model.counters)
--     , H.textarea_ [P.rows_ "30", P.cols_ "50"] [M.text $ M.ms $ show model]
--     ]
--  where
--   viewCounters :: [Counter] -> M.View Model Msg
--   viewCounters counters =
--     H.div_ [] (fmap viewCounter counters)
 
--   viewCounter :: Counter -> M.View Model Msg
--   viewCounter counter =
--     H.div_
--       []
--       [ H.button_ [H.onClick (MsgIncrement counter.counterId)] [M.text "+"]
--       , H.p_ [] [M.text $ M.ms $ show counter.count]
--       , H.button_ [H.onClick (MsgDecrement counter.counterId)] [M.text "-"]
--       , H.button_ [H.onClick (MsgReset counter.counterId)] [M.text "Reset"]
--       , H.button_ [H.onClick (MsgDelete counter.counterId)] [M.text "Delete"]
--       ]

