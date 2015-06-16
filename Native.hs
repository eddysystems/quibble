{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings #-}
-- | Native binary I/O

module Native
  ( Native(..)
  , save
  , load
  ) where

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word
import System.IO
import System.IO.Unsafe

class Native a where
  get :: Get a
  put :: a -> Put

instance Native Word8 where
  get = getWord8
  put = putWord8

instance Native Int8 where
  get = fromIntegral <$> getWord8
  put = putWord8 . fromIntegral

instance Native Int where
  get = fromIntegral <$> getWord64host
  put = putWord64host . fromIntegral

instance Native Int64 where
  get = fromIntegral <$> getWord64host
  put = putWord64host . fromIntegral

instance Native Float where
  get = getFloat32le
  put = putFloat32le

instance Native Double where
  get = getFloat64le
  put = putFloat64le

instance (V.Unbox a,Native a) => Native (Vector a) where
  get = do
    -- Taken from https://hackage.haskell.org/package/vector-binary-instances-0.2.1.0/docs/src/Data-Vector-Binary.html
    n <- get
    let lift = return . unsafePerformIO
    mv <- lift $ M.new n
    let fill i | i < n = do x <- get
                            (unsafePerformIO $ M.unsafeWrite mv i x) `seq` return ()
                            fill (i+1)
               | otherwise = return ()
    fill 0
    lift $ V.unsafeFreeze mv

  put x = do
    put $ V.length x
    V.mapM_ put x

instance (Native a,Native b) => Native (a,b) where
  get = do
    x <- get
    y <- get
    return (x,y)
  put (x,y) = put x >> put y

-- Load and unpack a binary blob
load :: Native a => Handle -> IO a
load f = do
  n <- runGet get <$> BL.hGet f 8
  --putStrLn $ "load "++show n
  runGet get <$> BL.hGet f n

-- Pack and write a binary blob
save :: Native a => Handle -> a -> IO ()
save f x = do
  let s = runPut (put x)
      n = BL.length s
  --putStrLn $ "save "++show n
  BL.hPut f $ runPut $ put n
  BL.hPut f s

