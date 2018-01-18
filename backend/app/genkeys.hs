{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where


import Data.Aeson (encode, ToJSON(..))
import           Data.ByteString.Lazy  (toStrict)
import           Jose.Jwa
import           Jose.Jwk
import           Jose.Jwt (KeyId(..))
import Data.Text
import qualified  Data.ByteString as BS

-- Get a Jwk
main :: IO ()
main = do
  putStrLn "Starting to generate keys..."
  (pubKey, privKey) <- generateRsaKeyPair 2048 (KeyId ("mykey" :: Text)) Enc (Just (Encrypted RSA_OAEP))
  let pubKeyJson = toStrict $ encode $ toJSON pubKey
      privKeyJson = toStrict $ encode $ toJSON privKey
  putStrLn "Finished generating keys, writing files now..."
  BS.writeFile "config/key.rsa.pub" pubKeyJson
  BS.writeFile "config/key.rsa" privKeyJson


