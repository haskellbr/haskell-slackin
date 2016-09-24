{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
import           Data.Text (Text)
import           Yesod

data App = App

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
    (widget, enctype) <- generateFormPost emailForm
    defaultLayout
        [whamlet|
                <h1>
                  Haskell Slackin
                <form method=post action=@{HomeR} enctype=#{enctype}>
                  ^{widget}
                  <button type=submit>Submit
        |]

slackInvite email = return ()

postHomeR :: Handler Html
postHomeR = do
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

main :: IO ()
main = warp 3000 App
