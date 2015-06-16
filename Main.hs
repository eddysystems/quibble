{-# LANGUAGE ScopedTypeVariables #-}
-- | Quibble toplevel

module Main where

import Format
import Options
import RNN
import Util
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Language.JavaScript.Parser
import System.IO

-- Options

data Opts = Opts
  { optNop :: Bool
  , optNN :: FilePath
  , optGPU :: Int
  }

instance Options Opts where
  defineOptions = pure Opts
    <*> defineOption optionType_bool (\o -> o { optionLongFlags = ["nop"], optionShortFlags = ['n'] })
    <*> defineOption optionType_string (\o -> o { optionLongFlags = ["model"], optionShortFlags = ['m'] })
    <*> defineOption optionType_int (\o -> o { optionLongFlags = ["gpu"], optionShortFlags = ['g'] })

quibble :: Opts -> FilePath -> IO ()
quibble opts path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  src <- TI.hGetContents h
  case parse (T.unpack src) "path" of
    Left e -> die e
    Right js -> do
      if optNop opts
      then putStr (renderToString js)
      else do
        nn <- load (optNN opts) (optGPU opts)
        TI.putStr $ format nn src js

main :: IO ()
main = runCommand $ \opts args -> case args of
  [path] -> quibble opts path
  _ -> die $ "Expected one argument, got " ++ show (length args)
