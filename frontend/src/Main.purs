module Main where

import Control.Apply
import Data.Functor
import Data.Map
import Data.Tuple hiding (lookup)
import Prelude
import Routing
import Routing.Hash
import Routing.Match
import Routing.Match.Class

import Control.Alt ((<|>))
import Control.Monad.Aff (launchAff_, liftEff', Aff, catchError)
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecodeJSON, genericEncode, genericEncodeJSON)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Network.HTTP.Affjax (AJAX, get,post)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, createClassStateless, createFactory)
import React.DOM as D
import ReactDOM (render)
import Network.HTTP.Affjax.Request
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson,class EncodeJson,(:=),(~>))
import Data.Argonaut.Core (jsonEmptyObject)

data Locations
  = Home
  | Login (Map String String) -- Callback url for oauth2
  | User


oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

home :: Match Locations
home = Home <$ lit ""

login :: Match Locations
login = Login <$> (homeSlash *> lit "login" *> params)

user :: Match Locations
user = User <$ (homeSlash *> lit "user")

routing :: Match Locations
routing =
  login    <|>
  user     <|>
  home

data AppProps = AppProps String

helloWorld :: ReactClass AppProps
helloWorld = createClassStateless helloText
  where
    helloText :: AppProps -> ReactElement
    helloText (AppProps text) = D.h1 [] [D.text text]

ui :: AppProps -> ReactElement
ui props = D.div' [ createFactory helloWorld props ]

appId :: forall eff. Eff (dom :: DOM | eff) (Maybe Element)
appId = do
  win <- window
  doc <- document win
  getElementById (ElementId "container") (documentToNonElementParentNode (htmlDocumentToDocument doc))

app :: forall eff. Eff (dom :: DOM | eff) Element
app = do
  appId' <- appId
  pure $ unsafePartial fromJust appId'

callService :: forall e. Aff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e) Unit
callService = do
  response <- get "http://localhost:3000/add/5/7?_accept=application/json"
  elem <- liftEff' app
  let content = ui $ AppProps response.response
  x <- liftEff' $ render content elem
  A.log (response.response )

data Code = Code { code :: String }

{- derive instance genericMyRecord :: Generic Code _
instance showCode :: Show Code where show = genericShow
instance encodeCode :: Encode Code where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true }) -}


instance encodeJsonCode :: EncodeJson Code where
  encodeJson (Code code)
     = "code" := code.code
    ~> jsonEmptyObject

requestLogin :: forall e. Code -> Aff (ajax :: AJAX, console :: CONSOLE | e) Unit
requestLogin code = do
  response <- post "http://localhost:3000/api/v1/login" $ encodeJson code
  A.log (response.response)

main :: forall e. Eff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e) Unit
main = launchAff_ $ do
  Tuple maybeOld new <- matchesAff routing `catchError` \_ -> pure (Tuple Nothing Home)
  case new of
    Home -> callService
    Login params -> case lookup "code" params of
      Just code -> requestLogin $ Code { code }
      Nothing -> A.log $ "login -- no code"
    User -> A.log $ "user"
