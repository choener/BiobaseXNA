{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import "PrimitiveArray" Data.Array.Repa.Index
import "PrimitiveArray" Data.Array.Repa.Shape
import Data.Char (toLower, toUpper)
import Data.Ix (Ix(..))
import Data.List as L
import Data.Primitive.Types
import Data.Tuple (swap)
import GHC.Base (remInt,quotInt)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Primary.Bounds



-- | Easy reading of a three-Char string into a triple.

threeChar :: String -> ExtPairAnnotation
threeChar s@[c,x,y]
  | Just c' <- L.lookup (toLower c) charCTList
  , Just x' <- L.lookup (toUpper x) charEdgeList
  , Just y' <- L.lookup (toUpper y) charEdgeList
  = (c',x',y')
  | map toLower s == "bif" = (unknownCT,unknownEdge,unknownEdge)
  | otherwise = error $ "can't convert string: " ++ s

-- | Each nucleotide in a pair may be paired using one of three edges:
-- watson-crik, sugar, or hoogsteen.

newtype Edge = Edge {unEdge :: Int}
  deriving (Eq,Ord,Ix)

instance (Shape sh,Show sh) => Shape (sh :. Edge) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.Edge 0
  unitDim = unitDim:.Edge 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.Edge n1) (sh2:.Edge n2) = addDim sh1 sh2 :. Edge (n1+n2) -- TODO will not necessarily yield a valid Edge
  size (sh1:.Edge n) = size sh1 * n
  sizeIsValid (sh1:.Edge n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.Edge sh2) (sh1':.Edge sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.Edge d) n = fromIndex ds (n `quotInt` d) :. Edge r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.Edge n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. Edge x
  deepSeq (sh:.n) x = deepSeq sh (n `seq` x)
  {-# INLINE rank #-}
  {-# INLINE zeroDim #-}
  {-# INLINE unitDim #-}
  {-# INLINE intersectDim #-}
  {-# INLINE addDim #-}
  {-# INLINE size #-}
  {-# INLINE sizeIsValid #-}
  {-# INLINE toIndex #-}
  {-# INLINE fromIndex #-}
  {-# INLINE inShapeRange #-}
  {-# INLINE listOfShape #-}
  {-# INLINE shapeOfList #-}
  {-# INLINE deepSeq #-}

(wc : sugar : hoogsteen : unknownEdge : edgeUndefined : _) = map Edge [0..]

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

instance (Shape sh,Show sh) => Shape (sh :. CTisomerism) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.CT 0
  unitDim = unitDim:.CT 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.CT n1) (sh2:.CT n2) = addDim sh1 sh2 :. CT (n1+n2) -- TODO will not necessarily yield a valid CT
  size (sh1:.CT n) = size sh1 * n
  sizeIsValid (sh1:.CT n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.CT sh2) (sh1':.CT sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.CT d) n = fromIndex ds (n `quotInt` d) :. CT r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.CT n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. CT x
  deepSeq (sh:.n) x = deepSeq sh (n `seq` x)
  {-# INLINE rank #-}
  {-# INLINE zeroDim #-}
  {-# INLINE unitDim #-}
  {-# INLINE intersectDim #-}
  {-# INLINE addDim #-}
  {-# INLINE size #-}
  {-# INLINE sizeIsValid #-}
  {-# INLINE toIndex #-}
  {-# INLINE fromIndex #-}
  {-# INLINE inShapeRange #-}
  {-# INLINE listOfShape #-}
  {-# INLINE shapeOfList #-}
  {-# INLINE deepSeq #-}

(cis : trans : unknownCT : undefinedCT : _) = map CT [0..]
antiCT = undefined
paraCT = undefined

charCTList =
  [ ('c',cis)
  , ('t',trans)
  , ('?',unknownCT)
  -- TODO antiCT, paraCT
  -- TODO '?' type (??? could denote bifurcation)
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



-- * Types

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



-- * little helpers

cWW = (cis,wc,wc)
cWS = (cis,wc,sugar)
cWH = (cis,wc,hoogsteen)
cSW = (cis,sugar,wc)
cSS = (cis,sugar,sugar)
cSH = (cis,sugar,hoogsteen)
cHW = (cis,hoogsteen,wc)
cHS = (cis,hoogsteen,sugar)
cHH = (cis,hoogsteen,hoogsteen)
tWW = (trans,wc,wc)
tWS = (trans,wc,sugar)
tWH = (trans,wc,hoogsteen)
tSW = (trans,sugar,wc)
tSS = (trans,sugar,sugar)
tSH = (trans,sugar,hoogsteen)
tHW = (trans,hoogsteen,wc)
tHS = (trans,hoogsteen,sugar)
tHH = (trans,hoogsteen,hoogsteen)


-- * special show instances

-- | This one requires ghc head
--
-- TODO maybe newtype this triple?

--instance Show (CTisomerism,Edge,Edge) where
--  show (ct,eI,eJ) = concat [show ct, show eI, show eJ]



-- * tuple-like selection

-- | Selection of nucleotides and/or type classes independent of which type we
-- are looking at.

class BaseSelect a b | a -> b where
  -- |  select first index or nucleotide
  baseL :: a -> b
  -- | select second index or nucleotide
  baseR :: a -> b
  -- | select both nucleotides as pair
  baseP :: a -> (b,b)
  -- | select basepair type if existing or return default cWW
  baseT :: a -> ExtPairAnnotation
  -- | update first index or nucleotide
  updL :: b -> a -> a
  -- | update second index or nucleotide
  updR :: b -> a -> a
  -- | update complete pair
  updP :: (b,b) -> a -> a
  -- | update basepair type, error if not possible due to type a
  updT :: ExtPairAnnotation -> a -> a

-- | extended pairtype annotation given

instance BaseSelect ((a,a),ExtPairAnnotation) a where
  baseL ((a,_),_) = a
  baseR ((_,b),_) = b
  baseP (lr   ,_) = lr
  baseT (_,t) = t
  updL n ((_,y),t) = ((n,y),t)
  updR n ((x,_),t) = ((x,n),t)
  updP n (_,t)     = (n,t)
  updT n (xy,_) = (xy,n)
  {-# INLINE baseL #-}
  {-# INLINE baseR #-}
  {-# INLINE baseP #-}
  {-# INLINE baseT #-}
  {-# INLINE updL #-}
  {-# INLINE updR #-}
  {-# INLINE updP #-}
  {-# INLINE updT #-}

-- | simple cis/wc-wc basepairs

instance BaseSelect (a,a) a where
  baseL (a,_) = a
  baseR (_,a) = a
  baseP = id
  baseT _ = cWW
  updL n (_,y) = (n,y)
  updR n (x,_) = (x,n)
  updP n _     = n
  updT n xy = if n==cWW then xy else error $ "updT on standard pairs can not update to: " ++ show n
  {-# INLINE baseL #-}
  {-# INLINE baseR #-}
  {-# INLINE baseP #-}
  {-# INLINE baseT #-}
  {-# INLINE updL #-}
  {-# INLINE updR #-}
  {-# INLINE updP #-}
  {-# INLINE updT #-}
