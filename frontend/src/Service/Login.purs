module Service.Login where

import Prelude

import Common (Host(..))
import Control.Apply (lift4)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Network.HTTP.Affjax (AJAX, post)
import Routing.Match.Class (params)

data LoginRequest = LoginRequest { provider :: String, code :: String , state :: String, scope :: String }

instance encodeJsonCode :: EncodeJson LoginRequest where
  encodeJson (LoginRequest auth)
     = "provider" := auth.provider ~>
       "code" := auth.code ~>
       "state" := auth.state ~>
       "scope" := auth.scope ~> jsonEmptyObject

paramsToLoginRequest :: (Map String String) -> Maybe LoginRequest
paramsToLoginRequest params = lift4 (\pr co st sc -> LoginRequest { provider : pr, code : co, state : st, scope : sc }) provider code state scope
  where provider = lookup "login" params
        code = lookup "code" params
        state = lookup "state" params
        scope = lookup "scope" params

requestLogin :: forall e. Host -> LoginRequest -> Aff (ajax :: AJAX, console :: CONSOLE | e) Unit
requestLogin (Host server) code = do
  response <- post (server <> "/api/v1/login") $ encodeJson code
  log (response.response)
