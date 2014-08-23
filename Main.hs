{-|
Module       : Main
Description  : Runs a simulation
Copyright    : None
License      : PublicDomain
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux-3.16

-}

module Main where

import           Control.Applicative
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import           Data.Yaml
import           Math.Castr.Agent
import           System.IO

data MainInput = MainInput { number :: Int
                           , heritability :: Double
                           , printer :: Printer
                           }

data Printer = JsonPretty
             | JsonUgly
             | Yaml

instance FromJSON MainInput where
  parseJSON (Object v) = MainInput <$> v .:? "n" .!= 1000
                                   <*> v .:? "h" .!= 0.5
                                   <*> v .:? "printer" .!= JsonPretty
  parseJSON _ = fail "Not an object"

instance FromJSON Printer where
  parseJSON (String s) = case s of
    "json-pretty" -> return JsonPretty
    "json-ugly" -> return JsonUgly
    "yaml" -> return Yaml
    _ -> fail "Invalid printer."
  parseJSON _ = fail "Not a string"

genPool :: Bs.ByteString -> IO ()
genPool inputBytes = case decodeEither inputBytes of
  Left err -> hPutStrLn stderr err
  Right (MainInput n h p) -> do
    pool <- mkPool n h
    case p of
      JsonPretty -> Bl.hPut stdout $ encodePretty pool
      JsonUgly -> Bl.hPut stdout $ Aeson.encode pool
      Yaml -> Bs.hPut stdout $ encode pool

main :: IO ()
main = hSetBinaryMode stdin True >> Bs.hGetContents stdin >>= genPool
