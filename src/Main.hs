{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import           Control.Monad.Logger
import qualified Data.Aeson             as Aeson
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector
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

data App = App { appPort              :: Int
               , appSlackOrganization :: String
               , appSlackToken        :: String
               , appSlackChan         :: TChan SlackState
               , appSlackState        :: TVar SlackState
               }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App where
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent $ do
            toWidgetHead [lucius|
                    .container {
                      margin-top: 20vh;
                      max-width: 500px;
                      text-align: center;
                      margin-bottom: 80px;
                    }

                    form input[type=email]:focus {
                      border-color: #76d176;
                    }

                    a {
                      color: #35A4BB;
                    }

                    button {
                      background-color: #76d176;
                      border-color: #76d176;
                    }

                    form .button-primary {
                      width: 100%;
                    }

                    form label {
                      display: none;
                    }

                    footer {
                      text-align: center;
                    }
            |]

            contents
        mmsg <- getMessage
        withUrlRenderer [hamlet|
            $doctype 5
            <html>
              <head>
                <title>#{title}
                <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/milligram/1.2.0/milligram.min.css" />
                ^{headTags}
              <body>
                $maybe msg <- mmsg
                  <div #message>#{msg}
                <div .container>
                  ^{bodyTags}
                <footer>
                  <small>
                    RODANDO EM HASKELL COM <a href="https://github.com/haskellbr/haskell-slackin">HASKELL-SLACKIN</a>
                  <br />
                  <small>
                    FEITO POR <a href="https://github.com/yamadapc">@yamadapc</a>
        |]

instance MonadLogger Handler where
    monadLoggerLog _ _ _ = return $ return ()

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

emailForm = renderDivs $
    areq emailField ("Email address" { fsAttrs = [("placeholder", "voce@haskellbr.com")]
                                     }) Nothing

getHomeR :: Handler Html
getHomeR = do
    App{..} <- getYesod
    SlackState{..} <- liftIO $ readTVarIO appSlackState
    webSockets websocketsHandler
    (widget, enctype) <- generateFormPost emailForm
    defaultLayout $ do
        [whamlet|
                <h1>
                  Entre no Slack da HaskellBR
                <form method=post action=@{HomeR} enctype=#{enctype}>
                  <fieldset>
                    ^{widget}
                  <button .button-primary type=submit>
                    Pedir um convite
                  <div .members> #{show ssMembers} já estão no chat
                  <div .online> #{show ssOnline} estão online agora
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
    ((result, _), _) <- runFormPost emailForm
    case result of
        FormSuccess email -> do
            slackInvite email
            defaultLayout [whamlet|
                                  <h1>
                                    Entre no Slack da HaskellBR

                                  <p>
                                    Um convite foi enviado para #{email}
                                  |]
        _ -> defaultLayout
            [whamlet|
                    <h1>
                      Haskell Slackin
                    <p>
                      Ocorreu um erro processando seu pedido...
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
    putStrLn "Fetching presence..."
    res <- getWith
        (defaults
            & param "token" .~ [ Text.pack appSlackToken ]
            & param "presence" .~ [ "1" ])
        (printf "https://%s.slack.com/api/users.list" appSlackOrganization)
    putStrLn "Fetched presence."
    let mmembers = res ^? responseBody . key "members" . _Array
    case mmembers of
        Just ms -> do
            let nmembers = length ms
                nonline = length
                    (Vector.filter
                        (\o -> o ^. key "presence" . _String == "active")
                        ms)
            atomically $ do
                let ss = SlackState nmembers nonline
                writeTChan appSlackChan ss
                writeTVar appSlackState ss
        _ -> print (res ^. responseBody)
    threadDelay (1000 * 1000 * 30)

main :: IO ()
main = do
    o <- getEnv "SLACK_ORGANIZATION"
    t <- getEnv "SLACK_TOKEN"
    p <- read . fromMaybe "3333" <$> lookupEnv "PORT"
    slackChan <- atomically newBroadcastTChan
    slackState <- atomically (newTVar (SlackState 0 0))
    let app = App p o t slackChan slackState

    _ <- forkIO $ slackWorker app
    _ <- forkIO $ do
        rchan <- atomically $ dupTChan slackChan
        forever $ do
            v <- atomically $ readTChan rchan
            print v

    warp p app
