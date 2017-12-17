{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Add where

import Import

getAddR :: Int -> Int -> Handler Value
getAddR x y = returnJson $ object ["result" .= z]
  where
    z = x + y