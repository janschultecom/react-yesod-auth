{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import System.Directory
import Data.Text
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid
import Authenticate.OAuth2
import Configuration.Dotenv.Types
import qualified Configuration.Dotenv as Dotenv

import           Data.Aeson            (Value)
import qualified Network.HTTP.Simple as H

-- * api

type Api = "oauth2" :> Capture "provider" Text :> QueryParam "code" Text :> Get '[PlainText] NoContent

myApi :: Proxy Api
myApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings

  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
	home <- getHomeDirectory
	_ <- Dotenv.loadFile $ Config [home <> "/.env"] [".env.example"] True
	oauth2 <- loadOAuthKeysEnv Google
	return $ serve myApi server

server :: Server Api
server = authenticate

{-
{
  "access_token":"1/fFAGRNJru1FTz70BzhT3Zg",
  "expires_in":3920,
  "token_type":"Bearer",
  "refresh_token":"1/xEoDL4iW3cxlI7yDbSRFYNG01kVKM2C-259HOF2aQbI"
}
-}
data OAuth2Response = OAuth2Response { accessToken :: Text, expiresIn :: Int, tokenType :: Text, refreshToken :: Text } deriving Show

-- We expect a JSON object, so we fail at any non-Object value.
instance FromJSON OAuth2Response where
    parseJSON = withObject "Occupation" $ \v -> OAuth2Response <$> v .: "access_token" <*> v .: "expires_in" <*> v .: "token_type" <*> v .: "refresh_token"

authenticate :: Text -> Maybe Text -> Handler NoContent
authenticate provider (Just code) = do
	_ <- liftIO $ print $ "Provider: " <> provider <> "\nCode: " <> code
	response <- H.httpJSONEither "http://www.mocky.io/v2/5a665dcf2e00006b29323e58" :: Handler (H.Response (Either H.JSONException OAuth2Response))
	_ <- liftIO $ print $ "Received response" <> show response
	return NoContent
authenticate _ _ = return NoContent

