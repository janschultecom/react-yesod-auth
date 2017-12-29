{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginSpec (spec) where

import Test.Hspec
--import Test.QuickCheck
import Data.Aeson
import Handler.Login
import TestImport

spec :: Spec
spec = --withApp $ do

    describe "Login" $ do
      it "parse a JSON login Request" $ do
         actual `shouldBe` expected
          where
            req = "{\"provider\":\"google\",\"code\":\"123\",\"state\":\"loggedin\",\"scope\":\"email\"}"
            actual = decode req
            expected = Just $ LoginRequest "google" "123" "loggedin" "email"

