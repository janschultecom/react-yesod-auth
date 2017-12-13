import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core

import Yesod.Static

main :: IO ()
main = do
           -- Get the static subsite, as well as the settings it is based on
           static@(Static settings) <- static "static"
           warp 3000 $ App static
