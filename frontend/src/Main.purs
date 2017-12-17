module Main where

import Prelude
import Control.Monad.Aff (launchAff_, liftEff')
import Control.Monad.Aff.Console as A
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Network.HTTP.Affjax (AJAX, get)
import React.DOM as D
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import React (ReactClass, ReactElement, createClassStateless, createFactory)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe, fromJust)
import ReactDOM (render)


data HelloProps = HelloProps String

helloWorld :: ReactClass HelloProps
helloWorld = createClassStateless helloText
  where
    helloText :: HelloProps -> ReactElement
    helloText (HelloProps text) = D.h1 [] [D.text text]

ui :: HelloProps -> ReactElement
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

main :: forall e. Eff (ajax :: AJAX , console :: CONSOLE, dom :: DOM | e) Unit
main = launchAff_ do
  response <- get "http://localhost:3000/add/5/7?_accept=application/json"
  elem <- liftEff' app
  let content = ui $ HelloProps response.response
  x <- liftEff' $ render content elem
  A.log (response.response )
