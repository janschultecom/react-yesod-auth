{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Authenticate where

import Import
import Data.Aeson
import GHC.Generics
import Web.Cookie
import Data.Time

getAuthenticateR :: String -> Handler ()
getAuthenticateR provider = do
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