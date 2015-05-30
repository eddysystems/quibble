{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables #-}
-- | Beam search over trees

module Search
  ( Score
  , NTree(..)
  , Tree(..)
  , beamSearch
  , label
  ) where

import Util
import Data.Either
import Data.List
import Data.Ord

-- Spine lazy tree with node labels n
data NTree n
  = NLeaf
  | NNode !n [NTree n]

-- Spine lazy tree with edges e, node labels n.
data Tree e n
  = Leaf
  | Node !n [(e,Tree e n)]

label :: NTree n -> Maybe n
label NLeaf = Nothing
label (NNode x _) = Just x

type Score = Double

-- Beam search some trees.
-- Very simple, but it worked for http://arxiv.org/pdf/1409.3215v3.pdf.
-- The results are returned in lazy unsorted order.
-- Can diverge if infinite strings are likely.
beamSearch :: Int -> [Tree Score a] -> [(Score,[a])]
beamSearch b ts = beam b $ map (\t -> (0,[],t)) ts

type Res a = (Score,[a])
type Cand a = (Score,[a],Tree Score a)

beam :: Int -> [Cand a] -> [Res a]
beam b ts = rs ++ beam b best where
  step :: Cand a -> Either (Res a) [Cand a]
  step (s,xs,Leaf) = Left (s,reverse xs)
  step (s,xs,Node x ts) = Right $ map (\(s',t) -> (s+s',x:xs,t)) ts

  (rs,next) = partitionEithers $ map step ts
  best = take b $ sortBy (comparing fst3) (concat next)
