
-- | Types for RNA secondary structure. Types vary from the simplest array
-- (D1Secondary) to rather complex ones.

module Biobase.Secondary.Diagrams where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Data.Aeson
import           Data.Binary
import           Data.List ((\\))
import           Data.List (sort,groupBy,sortBy,intersperse)
import           Data.List.Split (splitOn)
import           Data.Serialize
import           Data.Tuple.Select (sel1,sel2)
import           Data.Tuple (swap)
import           Data.Vector.Binary
import           Data.Vector.Serialize
import           GHC.Generics
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           Biobase.Primary.Nuc
import           Biobase.Secondary.Basepair



-- | RNA secondary structure with 1-diagrams. Each nucleotide is paired with at
-- most one other nucleotide. A nucleotide with index @k@ in @[0..len-1]@ is
-- paired if @unD1S VU.! k >=0 0@ Unpaired status is @-1@.

newtype D1Secondary = D1S {unD1S :: VU.Vector Int}
  deriving (Read,Show,Eq,Generic)

instance Binary    D1Secondary
instance Serialize D1Secondary
instance FromJSON  D1Secondary
instance ToJSON    D1Secondary

-- RNA secondary structure with 2-diagrams. Each nucleotide is paired with up
-- to two other nucleotides.

newtype D2Secondary = D2S {unD2S :: VU.Vector ( (Int,Edge,CTisomerism), (Int,Edge,CTisomerism) )}
  deriving (Read,Show,Eq,Generic)

instance Binary    D2Secondary
instance Serialize D2Secondary
instance FromJSON  D2Secondary
instance ToJSON    D2Secondary

-- | Conversion to and from 1-diagrams.

class MkD1Secondary a where
  mkD1S :: a -> D1Secondary
  fromD1S :: D1Secondary -> a

-- | Conversion to and from 2-diagrams.

class MkD2Secondary a where
  mkD2S :: a -> D2Secondary
  fromD2S :: D2Secondary -> a



-- * Tree-based representation
--
-- TODO Tree -> d1/2Secondary ?

-- | A secondary-structure tree. Has no notion of pseudoknots.

data SSTree idx a = SSTree   idx a [SSTree idx a]
                  | SSExtern Int a [SSTree idx a]
  deriving (Read,Show,Eq,Generic)

-- | Create a tree from (pseudoknot-free [not checked]) 1-diagrams.

d1sTree :: D1Secondary -> SSTree PairIdx ()
d1sTree s = ext $ sort ps where
  (len,ps) = fromD1S s
  ext [] = SSExtern len () []
  ext xs = SSExtern len () . map tree $ groupBy (\l r -> snd l > fst r) xs -- ">=" would be partial allowance for 2-diagrams
  tree [ij]    = SSTree ij () []
  tree (ij:xs) = SSTree ij () . map tree $ groupBy (\l r -> snd l > fst r) xs

-- | Create a tree from (pseudoknot-free [not checked]) 2-diagrams.

d2sTree :: D2Secondary -> SSTree ExtPairIdx ()
d2sTree s = ext $ sortBy d2Compare ps where
  (len,ps) = fromD2S s
  ext [] = SSExtern len () []
  ext xs = SSExtern len () . map tree . groupBy d2Grouping $ xs
  tree [ij]    = SSTree ij () []
  tree (ij:xs) = SSTree ij () . map tree . groupBy d2Grouping $ xs

d2Compare ((i,j),_) ((k,l),_)
  | i==k = compare l j
  | j==l = compare i k
  | otherwise = compare (i,j) (k,l)

d2Grouping ((i,j),_) ((k,l),_) = i<=k && j>=l

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
  mkD2S = D2S . VU.map (\k -> ((k,W,Cis),(-1,W,Cis))) . unD1S
  fromD2S (D2S xs) = D1S . VU.map (sel1 . sel1) $ xs

instance MkD2Secondary (Int,[ExtPairIdx]) where
  mkD2S (len,ps) = let xs = concatMap (\((i,j),(ct,e1,e2)) ->
                                          [ (i, (j,e1,ct))
                                          , (j, (i,e2,ct))
                                          ]) ps
                       f (x,y) z = if sel1 x == -1 then (z,y) else (x,z)
                   in D2S $ VU.accum f (VU.replicate len ((-1,W,Cis),(-1,W,Cis))) xs
  fromD2S (D2S s) = ( VU.length s
                    , let (xs,ys) = unzip . VU.toList $ s
                          g i j = let z = s VU.! i in if sel1 (sel1 z) == j then sel2 (sel1 z) else sel2 (sel2 z)
                          f (i,(j,eI,ct)) = ((i,j),(ct,eI,g j i))
                      in
                      map f . filter (\(i,(j,_,_)) -> i<j && j>=0) $ zip [0..] xs ++ zip [0..] ys
                    )



-- * Older instances (should still work)

-- | A second primitive generator, requiring dictionary and String. This one
-- generates pairs that are then used by the above instance. The dict is a list
-- of possible brackets: ["()"] being the minimal set.

instance MkD1Secondary ([String],String) where
  mkD1S (dict,xs) = mkD1S (length xs,ps) where
    ps :: [(Int,Int)]
    ps = unsafeDotBracket2pairlist dict xs
  fromD1S (D1S s) = (["()"], zipWith f [0..] $ VU.toList s) where
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
  mkD1S xs = mkD1S (["()" ::String],xs)
  fromD1S s = let (_::[String],res) = fromD1S s in res

instance MkD1Secondary (VU.Vector Char) where
  mkD1S xs = mkD1S (["()" ::String],xs)
  fromD1S s = let (_::[String],res::VU.Vector Char) = fromD1S s in res



-- * High-level parsing functionality for secondary structures

-- | Completely canonical structure.
--
-- TODO Check size of hairpins and interior loops?

isCanonicalStructure :: String -> Bool
isCanonicalStructure = all (`elem` "().")

-- | Is constraint type structure, i.e. there can also be symbols present
-- that denote up- or downstream pairing.

isConstraintStructure :: String -> Bool
isConstraintStructure = all (`elem` "().<>{}|")

-- | Take a structural string and split it into its constituents.
--
-- If we decide to /NOT/ depend on @lens@ explicitly, another way to write
-- this is:
--
-- @
-- structures :: forall p f . (Profunctor p, Functor f) => p [String] (f [String]) -> p String (f String)
-- structures = dimap (splitOn "&") (fmap (concat . intersperse "&"))
-- @

structures :: Iso' String [String]
structures = iso (splitOn "&") (concat . intersperse "&")

-- | A @fold@ structure is a single structure

foldStructure :: Prism' String String
foldStructure = prism id to where
  to s = case s^.structures of
           [t] -> Right t
           _   -> Left  s

-- | A @cofold@ structure has exactly two structures split by @&@ (which the
-- prism removes).

cofoldStructure :: Prism' String (String,String)
cofoldStructure = prism from to where
  from (l,r) = l ++ '&' : r
  to   s     = case s^.structures of
                 [l,r] -> Right (l,r)
                 _     -> Left  s

-- * Helper functions

-- | Secondary structure parser which allows pseudoknots, if they use different
-- kinds of brackets.

unsafeDotBracket2pairlist :: [String] -> String -> [(Int,Int)]
unsafeDotBracket2pairlist dict xs = sort . concatMap (f xs) $ dict where
  f xs [l,r] = g 0 [] . map (\x -> if x `elem` [l,r] then x else '.') $ xs where
    g :: Int -> [Int] -> String -> [(Int,Int)]
    g _ st [] = []
    g k st ('.':xs) = g (k+1) st xs
    g k sst (x:xs)
      | l==x = g (k+1) (k:sst) xs
    g k (s:st) (x:xs)
      | r==x = (s,k) : g (k+1) st xs
    g a b c = error $ show (a,b,c)

-- | Secondary structure parser with a notion of errors. We either return a
-- @Right@ structure, including flags, or a @Left@ error.

dotBracket2pairlist :: [String] -> String -> Either String ( [(Int,Int)] )
dotBracket2pairlist dict str = fmap (sort . concat) . sequence . map (f str) $ dict where
  f ys [l,r] = g 0 [] . map (\x -> if x `elem` [l,r] then x else '.') $ ys where
    g :: Int -> [Int] -> String -> Either String ( [(Int,Int)] )
    g _ [] [] = pure []
    g k st ('.':xs) = g (k+1) st xs
    g k st (x:xs) | l==x = g (k+1) (k:st) xs
    g k (s:st) (x:xs) | r==x = ((s,k):) <$> g (k+1) st xs
    g k [] xs = fail $ printf "too many closing brackets at position %d: '%s' (dot-bracket: %s)" k xs str
    g k st [] = fail $ printf "too many opening brackets, opening bracket(s) at: %s (dot-bracket: %s)" (show $ reverse st) str
    g a b c   = fail $ printf "unspecified error: %s (dot-bracket: %s)" (show (a,b,c)) str
  f xs lr@(_:_:_:_) = fail $ printf "unsound dictionary: %s (dot-bracket: %s)" lr str
  f xs lr     = fail $ printf "unspecified error: dict: %s, input: %s (dot-bracket: %s)" lr xs str

-- | Calculates the distance between two vienna strings.

viennaStringDistance :: Bool -> Bool -> String -> String -> (String,Int)
viennaStringDistance sPairs tPairs s t = (t,length $ ss++tt) where
  s' = either error id . dotBracket2pairlist ["()"] $ s
  t' = either error id . dotBracket2pairlist ["()"] $ t
  ss = if sPairs then s' \\ t' else []
  tt = if tPairs then t' \\ s' else []

