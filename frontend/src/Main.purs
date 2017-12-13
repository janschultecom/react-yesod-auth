module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console as A
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX, get)


{-import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest)

main = launchAff $ do
  res <- affjax $ defaultRequest { url = "https://httpbin.org/ip", method = Left GET }
  liftEff $ log $ "GET /api response: " <> res.response-}

main :: forall e. Eff (ajax :: AJAX , console :: CONSOLE | e) Unit
main = launchAff_ do
  response <- get "/add/5/7?_accept=application/json"
  A.log (response.response )
