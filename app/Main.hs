module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import           Data.String.Conv (toS)
import           Lib (compile, decode, match, optimise, sanitise)
import           System.Environment (getArgs)
import           System.IO (stdin, hIsEOF, hClose)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> fail "no pattern supplied"
    x : _ -> case decode (toS x) of
      Left err      -> putStrLn err
      Right pattern -> do
        let handle = stdin
        go (compile (optimise (sanitise pattern))) handle
  where
    go pattern handle = do
      done <- hIsEOF handle
      if done then hClose handle else do
        line <- BS.getLine
        case match pattern line of
          Nothing -> go pattern handle
          Just _  -> do
            Char8.putStrLn line
            go pattern handle
