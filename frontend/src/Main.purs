module Main where

import Authenticators (Authenticator(..), Provider(..), authRequestUrl)
import Common (Host(..))
import Components.Login (LoginProps(..), createLogin)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff_, liftEff')
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, try, error)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.History (DocumentTitle(DocumentTitle), URL(URL), pushState)
import DOM.HTML.Location (pathname, search)
import DOM.HTML.Types (HISTORY, htmlDocumentToDocument)
import DOM.HTML.Window (document, history, location)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson, class EncodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, post)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, Unit, bind, pure, unit, ($), (*>), (<$), (<$>), (<>))
import Process.Env as ENV
import React (ReactClass, ReactElement, createClassStateless, createFactory)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Routing (matches)
import Routing.Match (Match)
import Routing.Match.Class (lit, params)

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

login :: Match Locations
login = Login <$> (homeSlash *> lit "login" *> params)

user :: Match Locations
user = User <$ (homeSlash *> lit "user")

routing :: Match Locations
routing =
  login    <|>
  user     <|>
  home

data AppProps = AppProps Host


getContainer :: forall eff. Window -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Element
getContainer win = do
  doc <- document win
  maybeContainer <- getElementById (ElementId "container") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  case maybeContainer of
    Just container -> pure container
    Nothing -> throwException $ error "Couldn't find Container"

callService :: forall e. AppProps -> Window -> Eff (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM | e) Unit
callService (AppProps host) win = do
  --response <- get "http://localhost:3000/add/5/7?_accept=application/json"
  container <-  getContainer win
  let content = createLogin $ LoginProps host
  _ <- render content container
  log "Created Login"

data Code = Code { code :: String }

instance encodeJsonCode :: EncodeJson Code where
  encodeJson (Code code)
     = "code" := code.code
        ~> jsonEmptyObject

requestLogin :: forall e. Code -> Aff (ajax :: AJAX, console :: CONSOLE | e) Unit
requestLogin code = do
  response <- post "http://localhost:3000/api/v1/login" $ encodeJson code
  A.log (response.response)


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
          Just code -> requestLogin $ Code { code }
          Nothing -> A.log $ "login -- no code"
        User -> A.log $ "user"
