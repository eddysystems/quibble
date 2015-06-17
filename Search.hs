{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Beam search over trees

module Search
  ( Score
  , Tree(..)
  , beamSearch
  ) where

import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Debug.Trace

-- Spine lazy tree with edge labels a.
data Tree e = Tree !Bool [(e,Tree e)] -- Tree halting edges

type Width = Int
type Score = Float

-- Beam search some trees.
-- Very simple, but it worked for http://arxiv.org/pdf/1409.3215v3.pdf.
-- The results are returned in lazy unsorted order.
-- Can diverge if infinite strings are likely.
beamSearch :: (Show a,Ord a) => Width -> Tree (Score,a,Int) -> [(Score,[a])]
beamSearch w t = maybeToList (res c) ++ beam w Map.empty (Heap.singleton c) where
  c = Cand 0 [] 0 t

type Res a = (Score,[a])
data Cand a = Cand !Score [a] Int (Tree (Score,a,Int))

instance Eq a => Eq (Cand a) where
  (Cand s x n _) == (Cand s' x' n' _) = s == s' && x == x' && n == n'

instance Eq a => Ord (Cand a) where
  compare (Cand s _ _ _) (Cand s' _ _ _) = compare s s'

res :: Cand a -> Maybe (Res a)
res (Cand s xs _ (Tree True _)) = Just (s,reverse xs)
res _ = Nothing

-- We're beam searching on a sequence space, but the sequences we want to
-- compare for beam pruning have different lengths.  For example, if the
-- input is "console.log()", the sequences "c" and "  c" should be compared
-- because they both correspond to the same 'c' character.  Therefore, we
-- keep a map from input position to beam heaps.
type Beam a = Set (Cand a)
type Beams a = Map Int (Beam a)

-- If it fits, add a candidate to a beam set
add :: Ord a => Width -> Beams a -> Cand a -> Maybe (Beams a)
add w bs c@(Cand _ _ n _) = (\b -> Map.insert n b bs) <$> b' where
  b = Map.findWithDefault Set.empty n bs 
  b' = if Set.size b < w then Just (Set.insert c b)
       else if Set.findMax b > c then Just (Set.insert c (Set.deleteMax b))
       else Nothing

beam :: forall a. (Show a,Ord a) => Width -> Beams a -> MinHeap (Cand a) -> [Res a]
beam w beams heap = maybe [] rest (Heap.view heap) where
  rest :: (Cand a, MinHeap (Cand a)) -> [Res a]
  rest (Cand s xs n (Tree _ ts), heap) = catMaybes (map res next) ++ beam w beams' heap' where
    next = map (\((s',x,dn),t) -> Cand (s+s') (x:xs) (n+dn) t) ts'
    ts' = trace ("expanding "++show s++", "++show n++", "++show (reverse xs)) ts
    absorb (beams,heap) c = case add w beams c of
      Nothing -> (beams,heap)
      Just beams -> (beams,Heap.insert c heap)
    (beams',heap') = foldl' absorb (beams,heap) next
