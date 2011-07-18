{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Types for RNA secondary structure. Types vary from the simplest array
-- (D1Secondary) to rather complex ones.
--
-- TODO The complex ones are still coming in from other libraries.
--
-- TODO can we use Char8 instead of Char?
--
-- TODO prepare for extended RNA secondary structures!

{-# LANGUAGE RecordWildCards #-}

module Biobase.Secondary.Diagrams where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Read
import Data.List (sort,groupBy,sortBy)
import Data.Tuple.Select (sel1,sel2)
import Data.Tuple (swap)
import Control.Arrow

import Biobase.Primary
import Biobase.Secondary



-- | RNA secondary structure with 1-diagrams. Each nucleotide is paired with at
-- most one other nucleotide. A nucleotide with index k \in [0..len-1] is
-- paired if unD1S VU.! k > 0.

newtype D1Secondary = D1S {unD1S :: VU.Vector Int}
  deriving (Read,Show,Eq)

-- RNA secondary structure with 2-diagrams. Each nucleotide is paired with up
-- to two other nucleotides.

newtype D2Secondary = D2S {unD2S :: VU.Vector ( (Int,Edge,CTisomerism), (Int,Edge,CTisomerism) )}
  deriving (Read,Show,Eq)

-- |

class MkD1Secondary a where
  mkD1S :: a -> D1Secondary
  fromD1S :: D1Secondary -> a

-- |

class MkD2Secondary a where
  mkD2S :: a -> D2Secondary
  fromD2S :: D2Secondary -> a



-- * Tree-based representation
--
-- Tree -> d1/2Secondary ?

-- | A secondary-structure tree. Has no notion of pseudoknots.

data SSTree idx a = SSTree idx a [SSTree idx a]
                  | SSExt  Int a [SSTree idx a]
  deriving (Read,Show,Eq)

-- | Create a tree from (pseudoknot-free [not checked]) 1-diagrams.

d1sTree :: D1Secondary -> SSTree PairIdx ()
d1sTree s = ext $ sort ps where
  (len,ps) = fromD1S s
  ext [] = SSExt len () []
  ext xs = SSExt len () . map tree $ groupBy (\l r -> snd l > fst r) xs -- ">=" would be partial allowance for 2-diagrams
  tree [ij]    = SSTree ij () []
  tree (ij:xs) = SSTree ij () . map tree $ groupBy (\l r -> snd l > fst r) xs

-- | Create a tree from (pseudoknot-free [not checked]) 2-diagrams.

d2sTree :: D2Secondary -> SSTree ExtPairIdx ()
d2sTree s = ext $ sortBy d2Compare ps where
  (len,ps) = fromD2S s
  ext [] = SSExt len () []
  ext xs = SSExt len () . map tree . groupBy d2Grouping $ xs
  tree [ij]    = SSTree ij () []
  tree (ij:xs) = SSTree ij () . map tree . groupBy d2Grouping $ xs

d2Compare ((i,j),_) ((k,l),_)
  | i==k = compare l j
  | j==l = compare i k
  | otherwise = compare (i,j) (k,l)

d2Grouping ((i,j),_) ((k,l),_) = i<=k && j>=l

test :: (Int,[ExtPairIdx])
test = (20,test')

test' =
  [ ((2,15),(wc,wc,cis))
  , ((3,14),(wc,wc,cis))
  , ((4,13),(wc,wc,cis))
  , ((5,12),(wc,wc,cis))
  , ((6,10),(wc,hoogsteen,trans))
  , ((2,18),(sugar,sugar,trans))
  , ((15,18),(sugar,sugar,cis))
  ]


-- * Instances for D1S

-- | Conversion between D1S and D2S is lossy in D2S -> D1S

instance MkD1Secondary D2Secondary where
  mkD1S = fromD2S
  fromD1S = mkD2S

-- | (Length,List of Pairs)

instance MkD1Secondary (Int,[PairIdx]) where
  mkD1S (len,ps) = let xs = concatMap (\ij -> [ij,swap ij]) ps
                   in D1S $ VU.replicate len (-1) VU.// xs
  fromD1S (D1S s) = (VU.length s, filter (\(i,j) -> i<j && j>=0) . zip [0..] . VU.toList $ s)

-- * Instances for D2S

-- | Conversion between D1S and D2S is lossy in D2S -> D1S
--
-- TODO 'fromD2S' makes me wanna rewrite everything...

instance MkD2Secondary D1Secondary where
  mkD2S (D1S xs) = D2S . VU.map (\k -> ((k,wc,cis),(-1,unknownEdge,unknownCT))) $ xs
  fromD2S (D2S xs) = D1S . VU.map (sel1 . sel1) $ xs

instance MkD2Secondary (Int,[ExtPairIdx]) where
  mkD2S (len,ps) = let xs = concatMap (\((i,j),(e1,e2,ct)) ->
                                          [ (i, (j,e1,ct))
                                          , (j, (i,e2,ct))
                                          ]) ps
                       f (x,y) z = if sel1 x == -1 then (z,y) else (x,z)
                   in D2S $ VU.accum f (VU.replicate len ((-1,unknownEdge,unknownCT),(-1,unknownEdge,unknownCT))) xs
  fromD2S (D2S s) = ( VU.length s
                    , let (xs,ys) = unzip . VU.toList $ s
                          g i j = let z = s VU.! i in if sel1 (sel1 z) == j then sel2 (sel1 z) else sel2 (sel2 z)
                          f (i,(j,eI,ct)) = ((i,j),(eI,g j i,ct))
                      in
                      map f . filter (\(i,(j,_,_)) -> i<j && j>=0) $ zip [0..] xs ++ zip [0..] ys
                    )



-- * Older instances (should still work)

-- | A second primitive generator, requiring dictionary and String. This one
-- generates pairs that are then used by the above instance. The dict is a list
-- of possible brackets: ["()"] being the minimal set.
--
-- NOTE no dictionary is returned by "fromD1S".
--
-- TODO return dictionary that is actually seen?

instance MkD1Secondary ([String],String) where
  mkD1S (dict,xs) = mkD1S (length xs,ps) where
    ps :: [(Int,Int)]
    ps = dotBracket dict xs
  fromD1S (D1S s) = ([], zipWith f [0..] $ VU.toList s) where
    f k (-1) = '.'
    f k p
      | k>p = ')'
      | otherwise = '('

-- | Generate Secondary given that we have an unboxed vector of characters

instance MkD1Secondary ([String],VU.Vector Char) where
  mkD1S (dict,xs) = mkD1S (dict, VU.toList xs)
  fromD1S s = let (dict,res) = fromD1S s in (dict,VU.fromList res)

-- | A "fast" instance for getting the pair list of vienna-structures.

instance MkD1Secondary String where
  mkD1S xs = mkD1S (["()"],xs)
  fromD1S s = let (_::[String],res) = fromD1S s in res

instance MkD1Secondary (VU.Vector Char) where
  mkD1S xs = mkD1S (["()"],xs)
  fromD1S s = let (_::[String],res::VU.Vector Char) = fromD1S s in res



-- * Helper functions

-- | Secondary structure parser which allows pseudoknots, if they use different
-- kinds of brackets.

dotBracket :: [String] -> String -> [(Int,Int)]
dotBracket dict xs = sort . concatMap (f xs) $ dict where
  f xs [l,r] = g 0 [] . map (\x -> if x `elem` [l,r] then x else '.') $ xs where
    g :: Int -> [Int] -> String -> [(Int,Int)]
    g _ st [] = []
    g k st ('.':xs) = g (k+1) st xs
    g k sst (x:xs)
      | l==x = g (k+1) (k:sst) xs
    g k (s:st) (x:xs)
      | r==x = (s,k) : g (k+1) st xs
    g a b c = error $ show (a,b,c)
