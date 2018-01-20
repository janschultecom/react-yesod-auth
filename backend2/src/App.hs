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


authenticate :: Text -> Maybe Text -> Handler NoContent
authenticate provider (Just code) = do
	_ <- liftIO $ print $ "Provider: " <> provider <> "\nCode: " <> code
	return NoContent
authenticate _ _ = return NoContent

