{-# LANGUAGE DeriveGeneric #-}
module JsonLoader ( Asset(..), loadJSON ) where

import Data.Aeson
import GHC.Generics
import Control.Applicative
import qualified Data.ByteString.Lazy as BS

data Asset = Asset {
  vertexshader :: String,
  fragmentshader :: String,
  uvmap :: String,
  wavefront :: String,
  worldposition :: String,
  lightposition :: String,
  lightcolor :: String,
  uuid :: String
} deriving (Show, Generic)

instance FromJSON Asset

getJSON :: FilePath -> IO BS.ByteString
getJSON = BS.readFile 

loadJSON :: FilePath -> IO (Either String Asset)
loadJSON name = do
  result <- eitherDecode <$> getJSON name
  return $ case result of
    Left err -> Left err
    Right xs -> Right xs
