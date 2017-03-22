
-- | Secondary structure: define basepairs as Int-tuples, the three edges, a
-- nucleotide can use for pairing and the cis/trans isomerism. Both edges and
-- cis/trans come with a tag for "unknown".
--
-- Since we often want to make "pairedness" explicit, we have a newtype for
-- this as well.
--
-- TODO set ext-annotations to be (isomerism,edge,edge) and have a asString
-- instance to read "cWW" "tSH" and other notation.

module Biobase.Secondary.Basepair where

import           Data.Aeson
import           Data.Binary
import           Data.Char (toLower, toUpper)
import           Data.Ix (Ix(..))
import           Data.List as L
import           Data.Primitive.Types
import           Data.Serialize (Serialize)
import           Data.Tuple (swap)
import           Data.Vector.Fusion.Stream.Monadic (map,Step(..))
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           GHC.Generics
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import           Text.Read

import           Data.PrimitiveArray hiding (Complement(..),map)

import           Biobase.Primary
import           Biobase.Primary.Nuc.RNA
import           Biobase.Primary.Nuc



-- * Newtype for efficient basepair encoding.

-- | Encode a base pair as a single @Int@.

newtype Basepair = BP { getBP :: Int }
  deriving (Eq,Ord,Ix,Generic)

derivingUnbox "Basepair"
  [t| Basepair -> Int |] [| getBP |] [| BP |]

instance Binary    Basepair
instance Serialize Basepair
instance FromJSON  Basepair
instance ToJSON    Basepair

deriving instance Index Basepair

instance IndexStream z => IndexStream (z:.Basepair) where
  streamUp (ls:.BP l) (hs:.BP h) = flatten mk step $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ Done
            | otherwise = return $ Yield (z:.BP k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.BP l) (hs:.BP h) = flatten mk step $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ Done
            | otherwise = return $ Yield (z:.BP k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream Basepair

pattern AA   = BP  0
pattern AC   = BP  1
pattern AG   = BP  2
pattern AU   = BP  3
pattern CA   = BP  4
pattern CC   = BP  5
pattern CG   = BP  6
pattern CU   = BP  7
pattern GA   = BP  8
pattern GC   = BP  9
pattern GG   = BP 10
pattern GU   = BP 11
pattern UA   = BP 12
pattern UC   = BP 13
pattern UG   = BP 14
pattern UU   = BP 15
pattern NS   = BP 16
pattern NoBP = BP 17

{-
class MkBasepair a where
  mkBasepair :: a -> Basepair
  fromBasepair :: Basepair -> a

-- | If we get a "legal" base pair, we just create it, all other
-- combinations yield 'NoBP'. Non-standard base pairs have to be created
-- explicitly using @NS@. When going back to @a@, non-standard and no pair
-- yield @(N,N)@.

instance MkBasepair (Letter RNA,Letter RNA) where
  mkBasepair (l,r)
    | l >= A && l <= U && r >= A && r <= U
    = BP $ 4 * getLetter l + getLetter r
    | otherwise = NoBP
  fromBasepair k
    | k == NoBP || k == NS = (N,N)
    | otherwise = let (l,r) = getBP k `divMod` 4 in (Letter l, Letter r)
  {-# Inline mkBasepair #-}
  {-# Inline fromBasepair #-}
-}


-- * Newtypes for extended secondary structures

-- ** Encode which of three edges is engaged in base pairing

-- | Each nucleotide in a pair may be paired using one of three edges:
-- watson-crick, sugar, or hoogsteen.

newtype Edge = Edge {unEdge :: Int}
  deriving (Eq,Ord,Ix,Generic)

pattern W = Edge 0
pattern S = Edge 1
pattern H = Edge 2

instance Binary    Edge
instance Serialize Edge
instance FromJSON  Edge
instance ToJSON    Edge



-- | Human-readable Show instance.

instance Show Edge where
  show H = "H"
  show S = "S"
  show W = "W"

-- | Human-readable Read instance.

instance Read Edge where
  readPrec = parens $ do
    Ident s <- lexP
    return $ case s of
      "H" -> H
      "S" -> S
      "W" -> W
      _   -> error $ "read Edge: " ++ s

instance Bounded Edge where
  minBound = W
  maxBound = H

instance Enum Edge where
  toEnum   = Edge
  fromEnum = unEdge

derivingUnbox "Edge"
  [t| Edge -> Int |] [| unEdge |] [| Edge |]

-- ** Is the base pair in cis or trans configuration

-- | Nucleotides in a pairing may be in the cis(==?) or trans(==?) state.

newtype CTisomerism = CT {unCT :: Int}
  deriving (Eq,Ord,Ix,Generic)

pattern Cis = CT 0
pattern Trn = CT 1

instance Binary    CTisomerism
instance Serialize CTisomerism
instance FromJSON  CTisomerism
instance ToJSON    CTisomerism


-- | Human-readable Show instance.

instance Show CTisomerism where
  show Cis = "C"
  show Trn = "T"

-- | Human-readable Read instance.

instance Read CTisomerism where
  readPrec = parens $ do
    Ident s <- lexP
    return $ case s of
      "C" -> Cis
      "T" -> Trn
      _   -> error $ "read CTisomerism: " ++ s

instance Bounded CTisomerism where
  minBound = Cis
  maxBound = Trn

instance Enum CTisomerism where
  toEnum   = CT
  fromEnum = unCT

derivingUnbox "CTisomerism"
  [t| CTisomerism -> Int |] [| unCT |] [| CT |]



-- * Types

-- | A basepair is simply a pair of Ints which are 0-indexing a sequence.

type PairIdx = (Int,Int)

-- | A pair as a tuple containing 'Nuc's.

type Pair = (Letter RNA,Letter RNA)

-- | Annotation for a basepair.

type ExtPairAnnotation = (CTisomerism,Edge,Edge)

-- | An extended basepair is a basepair, annotated with edge and CTisomerism.

type ExtPairIdx = (PairIdx,ExtPairAnnotation)

-- | An extended basepair, with nucleotides an annotation.

type ExtPair = (Pair,ExtPairAnnotation)



-- * little helpers

pattern CHH = (Cis,H,H)
pattern CHS = (Cis,H,S)
pattern CHW = (Cis,H,W)
pattern CSH = (Cis,S,H)
pattern CSS = (Cis,S,S)
pattern CSW = (Cis,S,W)
pattern CWH = (Cis,W,H)
pattern CWS = (Cis,W,S)
pattern CWW = (Cis,W,W)

pattern THH = (Trn,H,H)
pattern THS = (Trn,H,S)
pattern THW = (Trn,H,W)
pattern TSH = (Trn,S,H)
pattern TSS = (Trn,S,S)
pattern TSW = (Trn,S,W)
pattern TWH = (Trn,W,H)
pattern TWS = (Trn,W,S)
pattern TWW = (Trn,W,W)

