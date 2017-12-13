{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Add where

import Import

getAddR :: Int -> Int -> Handler TypedContent
getAddR x y = selectRep $ do
    provideJson $ object ["result" .= z]
  where
    z = x + y