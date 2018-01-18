{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Authenticate where

import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Aeson                as A
import qualified Data.ByteString           as BS
import           Data.Time
import           GHC.Generics
import           Import
import           Prelude                   as P
import           Web.Cookie

import           Jose.Jwa
import           Jose.Jwe                  (jwkEncode)
import           Jose.Jwk                  (KeyUse (Enc), generateRsaKeyPair,
                                            generateSymmetricKey)
import           Jose.Jwt                  as J (encode)
import           Jose.Jwt                  (JwtEncoding (..), KeyId (..),
                                            Payload (..))

data Provider = Google deriving Show
newtype Code = Code Text deriving Show
newtype Scope = Scope Text deriving Show
newtype State = State Text deriving Show

data OAuth2 = OAuth2 Provider Code Scope State deriving Show

parse :: Text -> Maybe Provider
parse "google" = Just Google
parse _        = Nothing

extractOAuth2Params :: MonadHandler m => Text -> m (Maybe OAuth2)
extractOAuth2Params input = runMaybeT $ do
    provider <- MaybeT $ pure $ parse input
    code <- MaybeT $ lookupGetParam "code"
    scope <- MaybeT $ lookupGetParam "scope"
    state <- MaybeT $ lookupGetParam "state"
    return $ OAuth2 provider (Code code) (Scope scope) (State state)


getAuthenticateR :: Text -> Handler ()
getAuthenticateR provider = do

    bytes <- liftIO $ BS.readFile "config/key.pub"
    jwk <- let x = fmap fromJSON (decode $ fromStrict bytes) in
           case x of
              Just (Success y) -> pure y
              _                -> fail "Failed to parse key"



    maybeJWT <- lift $ jwkEncode RSA_OAEP A256GCM jwk (Claims "my secret shit")


    jwt <- case maybeJWT of
                Right j -> pure $ toStrict $ A.encode $ A.toJSON j
                Left ex -> fail $ "Couldn't create jwt: " <> show ex

    oauth2 <- extractOAuth2Params provider

    _ <- lift $ P.print oauth2
    c <- lift getCurrentTime
    let expires = addDays 30 $ utctDay c
        cookie = defaultSetCookie {
            setCookieName = "access_token",
            setCookieValue = jwt,
            setCookiePath = Just "/",
            setCookieDomain = Just "localhost",
            setCookieExpires = Just $ UTCTime expires (secondsToDiffTime 0)  }
    _ <- setCookie cookie
    redirectWith status302 ("http://localhost:4008/#/" :: Text) --"-- returnJson "bla" -- post -- Token { token = "123" }
