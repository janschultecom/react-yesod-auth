module Main where

import Routes

import Common (Host(..))
import Components.Login (LoginProps(..), createLogin)
import Control.Alt (void)
import Control.Monad.Aff (launchAff_, liftEff')
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY, Window)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Show (show)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, bind, pure, unit, ($), (<>))
import Process.Env as ENV
import React (ReactComponent)
import ReactDOM (render)
import Routing (matches)
import Service.Login as SL
import Util (redirectToHash, getContainer)

data AppProps = AppProps { client :: Host, server :: Host }

loginPage :: forall e. Host -> Window -> Eff (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM | e) (Maybe ReactComponent)
loginPage client win = do
  container <-  getContainer win
  render (createLogin $ LoginProps client) container

main :: forall e. Eff (ajax :: AJAX , history:: HISTORY, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION | e) Unit
main = do
  win <- window
  _ <- redirectToHash win

  let appProps = (AppProps { client : Host ENV.clientHost , server : Host ENV.serverHost } )
  failure <- try $ matches routing (\old new -> someAction old new win appProps)
  case failure of
    Left ex ->
      do
        _ <- log $ "Failed to match: " <> show ex
        someAction Nothing Home win appProps
    Right other -> pure unit --log "Finished"
        --- other stuff ---
  where
    someAction :: forall e2. Maybe Locations -> Locations -> Window -> AppProps -> Eff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e2) Unit
    someAction maybeOld new win (AppProps { client, server })= launchAff_ $ do
      x <- A.log $ "In do loop - before match. New: " <> (show new)
      case new of
        Home -> void $ liftEff' $ loginPage client  win
        Login params -> case SL.paramsToLoginRequest params of
          Just request ->
            do
              _ <- A.log "Matched login, performing login request now"
              SL.requestLogin server request
          Nothing -> A.log $ "login -- no code"
        User -> A.log $ "user"
