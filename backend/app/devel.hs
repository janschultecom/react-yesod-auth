{-# LANGUAGE PackageImports #-}
import "my-project" Application (develMain)
import Prelude (IO)

import Foundation
import Yesod.Static


main :: IO ()
main = do
          -- Get the static subsite, as well as the settings it is based on
          static@(Static settings) <- static "static"
          develMain $ App static
