module Util where

import Prelude

import Control.Monad.Aff (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import DOM (DOM)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Location (href, pathname, search)
import DOM.HTML.Types (HISTORY, Window, htmlDocumentToDocument)
import DOM.HTML.Window (document, history, location)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))


getContainer :: forall eff. Window -> Eff (dom :: DOM, exception :: EXCEPTION | eff) Element
getContainer win = do
  doc <- document win
  maybeContainer <- getElementById (ElementId "container") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  case maybeContainer of
    Just container -> pure container
    Nothing -> throwException $ error "Couldn't find Container"

redirectToHash :: forall e. Window -> Eff (history:: HISTORY, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION | e) Unit
redirectToHash win = do
  his <- history win
  doc <- document win
  loc <- location win
  _ <- href >=> log $ loc
  path <- pathname loc
  s <- search loc
  case path of
    "/" -> pushState (toForeign "") (DocumentTitle "") (URL $ "/#/" <> s) his
    _ -> pure unit
