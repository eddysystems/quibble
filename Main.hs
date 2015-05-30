{-# LANGUAGE ScopedTypeVariables #-}
-- | Quibble toplevel

module Main where

import Format
import Options
import Util
import Language.JavaScript.Parser
import System.IO

-- Options

data Opts = Opts
  { optNop :: Bool
  }

instance Options Opts where
  defineOptions = pure Opts
    <*> defineOption optionType_bool (\o -> o { optionLongFlags = ["nop"], optionShortFlags = ['n'] })

quibble :: Opts -> FilePath -> IO ()
quibble opts path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  src <- hGetContents h
  case parse src "path" of
    Left e -> die e
    Right js -> putStr $ if optNop opts
                         then renderToString js
                         else format src js

main :: IO ()
main = runCommand $ \opts args -> case args of
  [path] -> quibble opts path
  _ -> die $ "Expected one argument, got " ++ show (length args)
