{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson as Aeson
import           Data.Aeson.TH
import           Data.Aeson.Lens
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Network.Wreq
import           System.Environment
import           Text.Printf
import           Yesod
import           Yesod.WebSockets

data SlackState
    = SlackState { ssMembers :: Int
                 , ssOnline  :: Int
                 }
  deriving (Show)

deriveJSON defaultOptions ''SlackState

data App = App { appSlackOrganization :: String
               , appSlackToken        :: String
               , appSlackChan         :: TChan SlackState
               , appSlackState        :: TVar SlackState
               }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App where

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

emailForm = renderDivs $
    areq emailField "Email address" Nothing

getHomeR :: Handler Html
getHomeR = do
    App{..} <- getYesod
    SlackState{..} <- liftIO $ readTVarIO appSlackState
    webSockets websocketsHandler
    (widget, enctype) <- generateFormPost emailForm
    defaultLayout $ do
        [whamlet|
                <h1>
                  Haskell Slackin
                <form method=post action=@{HomeR} enctype=#{enctype}>
                  ^{widget}
                  <button type=submit>Submit
                <h3>
                  Total members:
                  <span .members> #{show ssMembers}
                <h3>
                  Online Now:
                  <span .online> #{show ssOnline}
        |]

        toWidgetBody [julius|
                var ws = new WebSocket('ws://localhost:3000');
                var membersEl = document.querySelector('.members');
                var onlineEl = document.querySelector('.online');
                ws.onmessage = function(event) {
                  var data = JSON.parse(event.data);
                  console.log(data);
                  membersEl.innerText = data.ssMembers;
                  onlineEl.innerText = data.ssOnline;
                };
               |]

slackInvite :: Text -> Handler ()
slackInvite email = do
    App{..} <- getYesod
    liftIO $ do
        res <- post
            (printf "https://%s.slack.com/api/users.admin.invite" appSlackOrganization)
            [ "email" := email
            , "token" := appSlackToken
            ]
        print (res ^. responseBody)

postHomeR :: Handler Html
postHomeR = do
    webSockets websocketsHandler
    ((result, widget), enctype) <- runFormPost emailForm
    case result of
        FormSuccess email -> do
            slackInvite email
            defaultLayout [whamlet|
                                  <h1>
                                    Haskell Slackin

                                  <p> Sent invitation to #{email}
                                  |]
        _ -> defaultLayout
            [whamlet|
                    <h1>
                      Haskell Slackin
            |]

websocketsHandler :: WebSocketsT Handler ()
websocketsHandler = do
    App{..} <- getYesod
    rchan <- liftIO $ atomically (dupTChan appSlackChan)
    forever $ do
        st <- liftIO $ atomically (readTChan rchan)
        sendTextData (Aeson.encode st)

slackWorker :: App -> IO ()
slackWorker App{..} = forever $ do
    putStrLn "Fetching presence"
    res <- getWith
        (defaults
            & param "token" .~ [ Text.pack appSlackToken ]
            & param "presence" .~ [ "1" ])
        (printf "https://%s.slack.com/api/users.list" appSlackOrganization)
    putStrLn "Fetched presence:"
    let Just members = res ^? responseBody . key "members" . _Array
        nmembers = length members
        nonline = length
            (Vector.filter
                (\o -> o ^. key "presence" . _String == "active")
                members)
    atomically $ do
        let ss = SlackState nmembers nonline
        writeTChan appSlackChan ss
        writeTVar appSlackState ss
    threadDelay (1000 * 1000 * 30)

main :: IO ()
main = do
    o <- getEnv "SLACK_ORGANIZATION"
    t <- getEnv "SLACK_TOKEN"
    slackChan <- atomically newBroadcastTChan
    slackState <- atomically (newTVar (SlackState 0 0))
    let app = App o t slackChan slackState

    forkIO $ slackWorker app

    forkIO $ do
        rchan <- atomically $ dupTChan slackChan
        forever $ do
            v <- atomically $ readTChan rchan
            print v

    warp 3000 app
