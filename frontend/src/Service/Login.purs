module Service.Login where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson, class EncodeJson, (:=), (~>))
import Network.HTTP.Affjax (AJAX, post)
import Common (Host(..))

data Code = Code { code :: String }

instance encodeJsonCode :: EncodeJson Code where
  encodeJson (Code code)
     = "code" := code.code
        ~> jsonEmptyObject

requestLogin :: forall e. Host -> Code -> Aff (ajax :: AJAX, console :: CONSOLE | e) Unit
requestLogin (Host server) code = do
  response <- post (server <> "/api/v1/login") $ encodeJson code
  log (response.response)
