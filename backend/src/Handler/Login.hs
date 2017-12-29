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

data LoginRequest = LoginRequest {
        provider :: Text,
        code :: Text,
        state :: Text,
        scope :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

postLoginR :: Handler Value
postLoginR = do
    post <- requireJsonBody :: Handler LoginRequest
    returnJson post -- Token { token = "123" }