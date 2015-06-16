{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings #-}
-- | char-rnn neural network interface

module RNN
  ( RNN
  , load
  , prob
  , prob0
  , step
  , eof
  ) where

import Prelude hiding (Show(..))
import qualified Prelude as P
import qualified Native as N
import Util
import Control.Monad
import Data.Char
import Data.Time.Clock
import Data.Word
import Debug.Trace
import System.IO
import System.Process
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Binary ()
import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)

data RNN = RNN
  { rIn :: !Handle
  , rOut :: !Handle
  , rVocab :: Map Char Word8
  , rState :: Vector Float
  , rSoftmax :: Vector Float }

load :: FilePath -> Int -> IO RNN
load checkpoint gpuid = do
  cwd <- getCurrentDirectory
  let cmd = (proc "luajit" [cwd </> "quibble.lua",checkpoint,"-gpuid",P.show gpuid])
            { cwd = Just "../char-rnn", std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  (Just sin, Just out, Nothing, _) <- createProcess cmd
  mapM_ (flip hSetBinaryMode True) [sin,out]
  (vocabCs,vocabIs) :: (Vector Word8,Vector Word8) <- N.load out
  unless (V.length vocabCs == V.length vocabIs) $
    die $ "Vocabulary size mismatch: " ++ P.show (V.length vocabCs) ++ " != " ++ P.show (V.length vocabIs)
  (state,softmax) <- N.load out
  return RNN { rIn = sin
             , rOut = out
             , rVocab = Map.fromList $ V.toList $ V.zip (V.map (chr . fromIntegral) vocabCs) vocabIs
             , rState = state
             , rSoftmax = softmax }

time :: Text -> IO a -> IO a
--time _ = id
time name io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- getCurrentTime
  TI.putStrLn $ T.unwords [name,"=",T.pack $ P.show (diffUTCTime t1 t0)]
  return x
{-
-}

--timeseq :: NFData a => Text -> a -> IO a
--timeseq name x = time name $ deepseq x (return x)

-- softmax :: [Float] -> [Float]
-- softmax xs = traceShow (xs,map (/ sum es) es) $ map (/ sum es) es where
--   es = map exp xs

prob :: RNN -> Char -> Float
prob nn x = case Map.lookup x (rVocab nn) of
  Nothing -> traceShow (rSoftmax nn) $ error $ "prob: weird character '" ++ showLitChar x "'"
  Just i -> rSoftmax nn ! fromIntegral (i - 1)

prob0 :: RNN -> Char -> Float
prob0 nn x = case Map.lookup x (rVocab nn) of
  Nothing -> 0
  Just i -> rSoftmax nn ! fromIntegral (i - 1)

step :: RNN -> Char -> RNN
step nn x = case Map.lookup x (rVocab nn) of
  Nothing -> error $ "step: weird character '" ++ showLitChar x "'"
  Just _ -> unsafePerformIO $ time "step" $ do
    time "write" $ N.save (rIn nn) $ V.cons (fromIntegral $ ord x) (rState nn)
    hFlush (rIn nn)
    (state,softmax) <- time "nn slurp" $ N.load (rOut nn)
    return nn { rState = state, rSoftmax = softmax }

eof :: Char
eof = chr 4
