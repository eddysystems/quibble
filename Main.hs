{-# LANGUAGE ScopedTypeVariables #-}
-- | Quibble toplevel

module Main where

import Control.Applicative
import Language.JavaScript.Parser
import Options
import Util
import System.IO

-- Options

data Opts = Opts {}

instance Options Opts where
  defineOptions = pure Opts

quibble :: FilePath -> IO ()
quibble path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  src <- hGetContents h
  case parse src "path" of
    Left e -> die e
    Right js -> putStr $ renderToString js

main :: IO ()
main = runCommand $ \(opts :: Opts) args -> case args of
  [path] -> quibble path
  _ -> die $ "Expected one argument, got " ++ show (length args)
