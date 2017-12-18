{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Login where

import Import
import Data.Aeson
import GHC.Generics

newtype Token = Token {
      token :: Text
    } deriving (Generic, Show)

instance ToJSON Token

postLoginR :: Handler Value
postLoginR = returnJson Token { token = "123" }