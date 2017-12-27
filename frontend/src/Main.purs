module Main where

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, catchError, launchAff_, liftEff', attempt)
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, catchException, try)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLElement (offsetHeight)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState, state)
import DOM.HTML.Location (href, pathname, search)
import DOM.HTML.Types (HISTORY, htmlDocumentToDocument)
import DOM.HTML.Window (document, history, location)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson, class EncodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign)
import Data.Functor (void)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, get, post)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, Unit, Void, bind, liftA1, pure, ($), (*>), (<$), (<$>), (<>),unit)
import Process.Env (googleClientId)
import React (ReactClass, ReactElement, createClassStateless, createFactory)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Routing (matches)
import Routing.Match (Match)
import Routing.Match.Class (fail, lit, str, params)
import Providers (Provider(..))

data Locations
  = Home
  | Login (Map String String) -- Callback url for oauth2
  | User

derive instance genericLocations :: Generic Locations _
instance showLocations :: Show Locations where show = genericShow

oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

home :: Match Locations
home = Home <$ lit ""

callback :: Match Locations
callback = Login <$> (str *> params)

login :: Match Locations
login = Login <$> (homeSlash *> lit "login" *> params)

user :: Match Locations
user = User <$ (homeSlash *> lit "user")


routing :: Match Locations
routing =
  callback <|>
  login    <|>
  user     <|>
  home

data AppProps = AppProps String

google :: String -> String
google clientId = "https://accounts.google.com/o/oauth2/v2/auth?\
 \scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&\
 \access_type=offline&\
 \include_granted_scopes=true&\
 \state=state_parameter_passthrough_value&\
 \redirect_uri=http%3A%2F%2Flocalhost:4008%2F?login=google&\
 \response_type=code&\
 \client_id=" <> googleClientId

data UIProps = UIProps { text :: String, clientId :: String }

helloWorld :: ReactClass UIProps
helloWorld = createClassStateless helloText
  where
    helloText :: UIProps -> ReactElement
    helloText (UIProps { text : t, clientId : cId }) = D.h1 [] [
      D.a [P.href $ google cId] [D.text "Login with google"],
      D.text t
      ]


ui :: UIProps -> ReactElement
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

callService :: forall e. AppProps -> Aff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e) Unit
callService (AppProps cId) = do
  --response <- get "http://localhost:3000/add/5/7?_accept=application/json"
  elem <- liftEff' app
  let content = ui $ UIProps { text : "HELLO", clientId : cId }
  x <- liftEff' $ render content elem
  --A.log (response.response )
  A.log "Elem"

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

handleError :: forall eff error old.
 Show error => error
              -> Aff
                   ( console :: CONSOLE
                   | eff
                   )
                   (Tuple (Maybe old) Locations)
handleError e = do
  x <- A.log $ show e
  pure (Tuple Nothing Home)

data WindowState = WindowState String

main :: forall e. Eff (ajax :: AJAX , history:: HISTORY, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION | e) Unit
main = do
  win <- window
  his <- history win
  loc <- location win
  path <- pathname loc
  s <- search loc
  _ <- case path of
    "/" -> pushState (toForeign "") (DocumentTitle "New page") (URL $ "/#/" <> s) his
    _ -> pure unit --log "All fine"
  failure <- try $ matches routing (\old new -> someAction old new)

  case failure of
    Left ex -> someAction Nothing Home
    Right other -> pure unit --log "Finished"
        --- other stuff ---
  where
    someAction :: forall e2. Maybe Locations -> Locations -> Eff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e2) Unit
    someAction maybeOld new = launchAff_ $ do
      x <- A.log $ "In do loop - before match. New: " <> (show new)
      case new of
        Home -> callService $ AppProps googleClientId
        Login params -> case lookup "code" params of
          Just code -> requestLogin $ Code { code }
          Nothing -> A.log $ "login -- no code"
        User -> A.log $ "user"
