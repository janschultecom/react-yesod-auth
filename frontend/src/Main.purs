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
import Control.Monad.Aff (launchAff_, liftEff', Aff)
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLElement (offsetHeight)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Int (floor)
import Data.Maybe (Maybe(Just,Nothing), fromJust)
import Network.HTTP.Affjax (AJAX, get)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, createClassStateless, createFactory)
import React.DOM as D
import ReactDOM (render)

data Locations
  = Home
  | Login (Map String String) -- Callback url for oauth2
  | User


oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

home :: Match Locations
home = Home <$ lit "/"

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

main :: forall e. Eff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e) Unit
main = launchAff_ $ do
  Tuple maybeOld new <- matchesAff routing
  A.log $ case new of
    Home -> "home"
    Login params -> case lookup "code" params of
      Just code -> "login -- " <> code
      Nothing -> "login -- no code"
    User -> "user"
