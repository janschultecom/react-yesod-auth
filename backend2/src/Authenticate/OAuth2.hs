{-# OverloadedStrings #-}

module Authenticate.OAuth2 where

import Data.Text
import Data.Monoid
import System.Environment

newtype ClientId = ClientId Text
newtype ClientSecret = ClientSecret Text

data Provider = Google

data OAuthKeys = OAuthKeys ClientId ClientSecret

envName :: Provider -> String
envName Google = "GOOGLE"

loadOAuthKeysEnv :: Provider -> IO OAuthKeys
loadOAuthKeysEnv provider = OAuthKeys
    <$> (fmap ClientId (getEnvT $ prefix <> "_CLIENT_ID"))
    <*> (fmap ClientSecret (getEnvT $ prefix <> "_CLIENT_SECRET"))

  where
		getEnvT = fmap pack . getEnv
		prefix = envName provider