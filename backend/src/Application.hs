{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core
import Yesod.Static

import Add
import Home

mkYesodDispatch "App" resourcesApp
