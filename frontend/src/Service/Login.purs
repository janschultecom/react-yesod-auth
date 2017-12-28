module Service.Login where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE,log)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson, class EncodeJson, (:=), (~>))
import Network.HTTP.Affjax (AJAX, post)

data Code = Code { code :: String }

instance encodeJsonCode :: EncodeJson Code where
  encodeJson (Code code)
     = "code" := code.code
        ~> jsonEmptyObject

requestLogin :: forall e. Code -> Aff (ajax :: AJAX, console :: CONSOLE | e) Unit
requestLogin code = do
  response <- post "http://localhost:3000/api/v1/login" $ encodeJson code
  log (response.response)
