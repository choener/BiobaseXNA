{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Secondary structure: define basepairs as Int-tuples, the three edges, a
-- nucleotide can use for pairing and the cis/trans isomerism. Both edges and
-- cis/trans come with a tag for "unknown".
--
-- TODO set ext-annotations to be (isomerism,edge,edge) and have a asString
-- instance to read "cWW" "tSH" and other notation.

module Biobase.Secondary where

import Data.Primitive.Types
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Ix (Ix(..))
import Data.Tuple (swap)
import Data.List as L
import Data.Char (toLower, toUpper)

import Biobase.Primary
import Biobase.Primary.Bounds



-- | Easy reading of a three-Char string into a triple.

threeChar :: String -> ExtPairAnnotation
threeChar s@[c,x,y]
  | Just c' <- L.lookup (toLower c) charCTList
  , Just x' <- L.lookup (toUpper x) charEdgeList
  , Just y' <- L.lookup (toUpper y) charEdgeList
  = (c',x',y')
  | otherwise = error $ "can't convert string: " ++ s

-- | A basepair is simply a pair of Ints which are 0-indexing a sequence.
--
-- TODO storable vector, newtype, peek/poke?

type PairIdx = (Int,Int)

-- | A pair as a tuple containing 'Nuc's.

type Pair = (Nuc,Nuc)

-- | Annotation for a basepair.

type ExtPairAnnotation = (CTisomerism,Edge,Edge)

-- | An extended basepair is a basepair, annotated with edge and CTisomerism.

type ExtPairIdx = (PairIdx,ExtPairAnnotation)

-- | An extended basepair, with nucleotides an annotation.

type ExtPair = (Pair,ExtPairAnnotation)

-- | Each nucleotide in a pair may be paired using one of three edges:
-- watson-crik, sugar, or hoogsteen.

newtype Edge = Edge {unEdge :: Int}
  deriving (Eq,Ord,Ix)

(wc : sugar : hoogsteen : unknownEdge : _) = map Edge [0..]

charEdgeList =
  [ ('W',wc)
  , ('S',sugar)
  , ('H',hoogsteen)
  , ('?',unknownEdge)
  ]

edgeCharList = map swap charEdgeList

-- | Human-readable Show instance.

instance Show Edge where
  show k
    | Just v <- k `lookup` edgeCharList = [v]
    | otherwise = "?"

-- | Human-readable Read instance.

instance Read Edge where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x ==' ' = readsPrec p xs
    | Just n <- x `lookup` charEdgeList = [(n,xs)]
    | otherwise = []

-- | Nucleotides in a pairing may be in the cis(==?) or trans(==?) state.

newtype CTisomerism = CT {unCT :: Int}
  deriving (Eq,Ord,Ix)

(cis : trans : unknownCT : _) = map CT [0..]
antiCT = undefined
paraCT = undefined

charCTList =
  [ ('c',cis)
  , ('t',trans)
  -- TODO antiCT, paraCT
  ]

ctCharList = map swap charCTList

-- | Human-readable Show instance.

instance Show CTisomerism where
  show k
    | Just v <- k `lookup` ctCharList = [v]
    | otherwise = "?"

-- | Human-readable Read instance.

instance Read CTisomerism where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x ==' ' = readsPrec p xs
    | Just n <- x `lookup` charCTList = [(n,xs)]
    | otherwise = []



-- * Instances

-- ** Instances for 'Edge'

deriving instance Prim Edge
deriving instance VGM.MVector VU.MVector Edge
deriving instance VG.Vector VU.Vector Edge
deriving instance VU.Unbox Edge

instance Bounded Edge where
  minBound = wc
  maxBound = unknownEdge

instance Bounds Edge where
  minNormal = wc
  maxNormal = wc
  minExtended = wc
  maxExtended = hoogsteen

instance Enum Edge where
  toEnum   = Edge
  fromEnum = unEdge

-- ** Instances for 'CTisomerism'

deriving instance Prim CTisomerism
deriving instance VGM.MVector VU.MVector CTisomerism
deriving instance VG.Vector VU.Vector CTisomerism
deriving instance VU.Unbox CTisomerism

instance Bounded CTisomerism where
  minBound = cis
  maxBound = unknownCT

instance Bounds CTisomerism where
  minNormal = cis
  maxNormal = cis
  minExtended = cis
  maxExtended = trans

instance Enum CTisomerism where
  toEnum   = CT
  fromEnum = unCT

-- ** special show instances

-- | This one requires ghc head
--
-- TODO maybe newtype this triple?

--instance Show (CTisomerism,Edge,Edge) where
--  show (ct,eI,eJ) = concat [show ct, show eI, show eJ]
