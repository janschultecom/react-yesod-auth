module Main where

import Common (Host(..))
import Components.Login (LoginProps(..), createLogin)
import Control.Monad.Aff (launchAff_, liftEff')
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY, Window)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Show (show)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, bind, pure, unit, ($), (<>))
import Process.Env as ENV
import ReactDOM (render)
import Routing (matches)
import Util (redirectToHash,getContainer)
import Service.Login as SL
import Routes
data AppProps = AppProps Host


callService :: forall e. AppProps -> Window -> Eff (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM | e) Unit
callService (AppProps host) win = do
  --response <- get "http://localhost:3000/add/5/7?_accept=application/json"
  container <-  getContainer win
  let content = createLogin $ LoginProps host
  _ <- render content container
  log "Created Login"

main :: forall e. Eff (ajax :: AJAX , history:: HISTORY, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION | e) Unit
main = do
  win <- window
  _ <- redirectToHash win
  failure <- try $ matches routing (\old new -> someAction old new win)

  case failure of
    Left ex -> someAction Nothing Home win
    Right other -> pure unit --log "Finished"
        --- other stuff ---
  where
    someAction :: forall e2. Maybe Locations -> Locations -> Window -> Eff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e2) Unit
    someAction maybeOld new win = launchAff_ $ do
      x <- A.log $ "In do loop - before match. New: " <> (show new)
      case new of
        Home -> liftEff' $ callService (AppProps (Host ENV.clientHost)) win
        Login params -> case lookup "code" params of
          Just code -> SL.requestLogin $ SL.Code { code }
          Nothing -> A.log $ "login -- no code"
        User -> A.log $ "user"
