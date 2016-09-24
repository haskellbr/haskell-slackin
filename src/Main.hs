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
    webSockets websocketsHandler
    (widget, enctype) <- generateFormPost emailForm
    defaultLayout $ do
        toWidgetBody [julius|
                var ws = new WebSocket('//localhost:3000');
                ws.onmessage = function(event) {
                  console.log(event);
                };
               |]
        [whamlet|
                <h1>
                  Haskell Slackin
                <form method=post action=@{HomeR} enctype=#{enctype}>
                  ^{widget}
                  <button type=submit>Submit
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

slackWorker :: App -> TChan SlackState -> IO ()
slackWorker App{..} slackState = forever $ do
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
    atomically (writeTChan slackState (SlackState nmembers nonline))
    threadDelay (1000 * 1000 * 30)

main :: IO ()
main = do
    o <- getEnv "SLACK_ORGANIZATION"
    t <- getEnv "SLACK_TOKEN"
    slackChan <- atomically newBroadcastTChan
    let app = App o t slackChan

    forkIO $ slackWorker app slackChan

    forkIO $ do
        rchan <- atomically $ dupTChan slackChan
        forever $ do
            v <- atomically $ readTChan rchan
            print v

    warp 3000 app
