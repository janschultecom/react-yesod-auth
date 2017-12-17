{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "React/Purescript + Yesod/Haskell"
    --addScriptRemote "https://unpkg.com/react@16/umd/react.development.js"
    --addScriptRemote "https://unpkg.com/react-dom@16/umd/react-dom.development.js"
    --addScriptRemote "https://unpkg.com/babel-standalone@6.15.0/babel.min.js"
    addScript $ StaticR js_main_js
    [whamlet|
        <div class="container">Hello World!
    |]

