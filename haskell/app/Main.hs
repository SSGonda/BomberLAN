-- =-----------------------------------=
--          NECESSARY IMPORTS
-- =-----------------------------------=

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE NoFieldSelectors #-}
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
import Data.Aeson (FromJSON)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import qualified Miso as M
import Miso.Fetch (Response (..))
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Text.Read (readMaybe)
import Data.Function ((&))
-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
import           Control.Monad (unless)
import           GHC.Generics
-----------------------------------------------------------------------------
-- import           Miso hiding (on)
import           Miso.Lens
import           Miso.WebSocket
import           Miso.String (ToMisoString, MisoString)
import qualified Miso.String as MS

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
    putStrLn "Server Running"
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
          pure $ Append (Message dateString message SERVER)
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
mainClient = M.run $ M.startApp (websocketComponent 0)