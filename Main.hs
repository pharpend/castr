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

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as Bl
import           Data.Monoid
import           Math.Castr.Agent
import           System.Environment
import           System.IO

main :: IO ()
main = runArguments =<< getArgs

-- |I can't get any command line parsers to work correctly, so I'm
-- doing it by hand
help :: IO ()
help = Bl.hPut stdout . mappend
  "Things you can do are listed. Unless otherwise noted, all output \n\
  \is in compressed JSON.\n" .
  mconcat . map (mappend "\n\t") $
    [ "mkpool N H\tGenerate a pool with N agents, a heritability of H."
    , "splpool F GS\tSplit the pool read from F into groups of size GS."
    , "\n"
    ]
  
runArguments :: [String] -> IO ()
runArguments args = case args of
    ["mkpool", n, h] -> printPool =<< mkPool (read n) (read h)
    ["splpool", fpath, groupSize] -> splPool fpath (read groupSize)
    _ -> help

printPool :: Pool -> IO ()
printPool = Bl.hPut stdout . encodePretty

splPool :: FilePath -> Int -> IO ()
splPool fpath subsize = do
  poolBytes <- Bl.readFile fpath
  case eitherDecode poolBytes of
    Left errorMessage -> hPutStr stderr errorMessage
    Right pl -> do
      Bl.hPut stdout . encodePretty $ splitPool subsize pl
