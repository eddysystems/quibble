{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, BangPatterns #-}
-- | Duck utility functions

module Util
  (
  -- * IO
    exit
  , die
  , fputs
  , puts
  -- * Functionals
  , (...), uncurry3
  , first, second
  , left, right
  -- * Monad
  , nop
  , (>.), (>.=), (>=.)
  , (<<), (.<), (=.<), (.=<)
  , foldM1
  , zipWith3M
  -- * Misc
  , fst3, snd3, thd3
  , trimWhile
  -- * Timing
  , time
  , timeseq
  ) where

import Control.DeepSeq
import Data.Time.Clock
import System.IO
import System.Exit
import Control.Monad
import Foreign.C.String

-- |Write a string to a stream all at once.
--
-- The string is written out all at once (as if with fputs in C), so it
-- won't end up interleaved with other strings like 'putStrLn' does.
fputs :: Handle -> String -> IO ()
fputs h s = withCStringLen s (uncurry $ hPutBuf h)

-- |Write a string to stdout with a trailing newline.
puts :: String -> IO ()
puts s = fputs stdout (s++"\n")

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) f g x y = f (g x y)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

zipWith3M :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f x y z = mapM (uncurry3 f) (zip3 x y z)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

-- more efficient than Arrow instances:
first :: (a -> c) -> (a,b) -> (c,b)
first f (a,b) = (f a,b)
second :: (b -> c) -> (a,b) -> (a,c)
second f (a,b) = (a,f b)

left :: (a -> c) -> Either a b -> Either c b
left f (Left a) = Left (f a)
left _ (Right b) = Right b
right :: (b -> c) -> Either a b -> Either a c
right _ (Left a) = Left a
right f (Right b) = Right (f b)

exit :: Int -> IO a
exit 0 = exitSuccess
exit i = exitWith (ExitFailure i)

-- Some convenient extra monad operators

infixl 1 >., >.=, >=.
infixr 1 <<, .<, =.<, .=<
(>.) :: Monad m => m a -> b -> m b
(.<) :: Monad m => b -> m a -> m b
(<<) :: Monad m => m b -> m a -> m b
(>.=) :: Monad m => m a -> (a -> b) -> m b
(=.<) :: Monad m => (a -> b) -> m a -> m b
(>=.) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(.=<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c

(>.) e r = e >> return r
(.<) r e = e >> return r
(<<) r e = e >> r
(>.=) e r = e >>= return . r
(=.<) r e = e >>= return . r -- fmap, <$>, liftM
(>=.) e r = e >=> return . r
(.=<) r e = e >=> return . r

nop :: Monad m => m ()
nop = return ()

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (h:t) = foldM f h t
foldM1 _ [] = error "foldM1 applied to an empty list"

trimWhile :: (a -> Bool) -> [a] -> [a]
trimWhile f = dropWhile f . reverse . dropWhile f . reverse

time :: String -> IO a -> IO a
time name io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- getCurrentTime
  putStrLn $ name ++ " = " ++ show (diffUTCTime t1 t0)
  return x

timeseq :: NFData a => String -> a -> IO a
timeseq name x = time name $ deepseq x (return x)
