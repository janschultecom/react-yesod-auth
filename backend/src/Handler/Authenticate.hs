{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Authenticate where

import Import
import Data.Aeson
import GHC.Generics
import Web.Cookie
import Data.Time
import Prelude as P
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)


data Provider = Google deriving Show
newtype Code = Code Text deriving Show
newtype Scope = Scope Text deriving Show
newtype State = State Text deriving Show

data OAuth2 = OAuth2 Provider Code Scope State deriving Show

parse :: Text -> Maybe Provider
parse "google" = Just Google
parse _ = Nothing

extractOAuth2Params :: MonadHandler m => Text -> m (Maybe OAuth2)
extractOAuth2Params input = runMaybeT $ do
    provider <- MaybeT $ pure $ parse input
    code <- MaybeT $ lookupGetParam "code"
    scope <- MaybeT $ lookupGetParam "scope"
    state <- MaybeT $ lookupGetParam "state"
    return $ OAuth2 provider (Code code) (Scope scope) (State state)


getAuthenticateR :: Text -> Handler ()
getAuthenticateR provider = do
    --triple <- (\a b c -> (a,b,c)) <$> lookupGetParam "code" <*> lookupGetParam "scope" <*> lookupGetParam "state"
    --triple <- liftA3 (\a b c -> (a,b,c)) (lookupGetParam "code") (lookupGetParam "scope") (lookupGetParam "state")
    oauth2 <- extractOAuth2Params provider

    _ <- lift $ P.print oauth2
    c <- lift getCurrentTime                  -- 2009-04-21 14:25:29.5585588 UTC
    let --(y,m,d) = toGregorian $ utctDay c    -- (2009,4,21)
        expires = addDays 30 $ utctDay c
        cookie = defaultSetCookie {
            setCookieName = "Authenticate",
            setCookieValue = "cookieValue",
            setCookiePath = Just "/",
            setCookieExpires = Just $ UTCTime expires (secondsToDiffTime 0)  }
    _ <- setCookie cookie
    redirectWith status302 ("http://localhost:4008/#/" :: Text) --"-- returnJson "bla" -- post -- Token { token = "123" }