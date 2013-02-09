{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Encoding of Watson-Crick and Wobble Pairs in the Vienna RNA package style.

module Biobase.Secondary.Vienna where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Ix
import Data.Primitive.Types
import Data.Tuple (swap)
import GHC.Base (remInt,quotInt)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Data.Array.Repa.ExtShape
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import Biobase.Primary
import Biobase.Primary.Bounds



-- | Use machine Ints internally

newtype ViennaPair = ViennaPair Int
  deriving (Eq,Ord,Ix)

instance (Shape sh,Show sh) => Shape (sh :. ViennaPair) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.ViennaPair 0
  unitDim = unitDim:.ViennaPair 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.ViennaPair n1) (sh2:.ViennaPair n2) = addDim sh1 sh2 :. ViennaPair (n1+n2) -- TODO will not necessarily yield a valid ViennaPair
  size (sh1:.ViennaPair n) = size sh1 * n
  sizeIsValid (sh1:.ViennaPair n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.ViennaPair sh2) (sh1':.ViennaPair sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.ViennaPair d) n = fromIndex ds (n `quotInt` d) :. ViennaPair r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.ViennaPair n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. ViennaPair x
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

instance (Eq sh, Shape sh, Show sh, ExtShape sh) => ExtShape (sh :. ViennaPair) where
  subDim (sh1:.ViennaPair n1) (sh2:.ViennaPair n2) = subDim sh1 sh2 :. (ViennaPair $ n1-n2)
  rangeList (sh1:.ViennaPair n1) (sh2:.ViennaPair n2) = [sh:.ViennaPair n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2)]]

(vpNP:vpCG:vpGC:vpGU:vpUG:vpAU:vpUA:vpNS:vpUndefined:_) = map ViennaPair [0..]

class MkViennaPair a where
  mkViennaPair :: a -> ViennaPair
  fromViennaPair :: ViennaPair -> a

instance MkViennaPair (Nuc,Nuc) where
  mkViennaPair (b1,b2) -- = viennaPairTable `PA.index` (Z:.b1:.b2)
    | b1==nC&&b2==nG = vpCG
    | b1==nG&&b2==nC = vpGC
    | b1==nG&&b2==nU = vpGU
    | b1==nU&&b2==nG = vpUG
    | b1==nA&&b2==nU = vpAU
    | b1==nU&&b2==nA = vpUA
    | otherwise = vpNS
  {-# INLINE mkViennaPair #-}
  fromViennaPair p
    | p==vpCG = (nC,nG)
    | p==vpGC = (nG,nC)
    | p==vpGU = (nG,nU)
    | p==vpUG = (nU,nG)
    | p==vpAU = (nA,nU)
    | p==vpUA = (nU,nA)
    | otherwise = error "non-standard pairs can't be backcasted"
  {-# INLINE fromViennaPair #-}

viennaPairTable :: U (Z:.Nuc:.Nuc) ViennaPair
viennaPairTable = fromAssocs (Z:.nN:.nN) (Z:.nU:.nU) vpNS
  [ (Z:.nC:.nG , vpCG)
  , (Z:.nG:.nC , vpGC)
  , (Z:.nG:.nU , vpGU)
  , (Z:.nU:.nG , vpUG)
  , (Z:.nA:.nU , vpAU)
  , (Z:.nU:.nA , vpUA)
  ]
{-# NOINLINE viennaPairTable #-}

deriving instance VGM.MVector VU.MVector ViennaPair
deriving instance VG.Vector VU.Vector ViennaPair
deriving instance VU.Unbox ViennaPair
deriving instance Prim ViennaPair

instance Enum ViennaPair where
  toEnum x
    | x>=0 && x<=7 = ViennaPair x
    | otherwise    = error $ "can't make to enum" ++ show x
  fromEnum (ViennaPair x) = x
  {-# INLINE toEnum #-}
  {-# INLINE fromEnum #-}

instance Bounded ViennaPair where
  minBound = vpNP
  maxBound = vpNS

instance Bounds ViennaPair where
  minNormal = vpCG
  maxNormal = vpUA
  minExtended = vpNP
  maxExtended = vpNS

instance Show ViennaPair where
  show x
    | Just s <- x `lookup` pairToString = s
    | otherwise = "??"

instance Read ViennaPair where
  readsPrec p [] = []
  readsPrec p [x] = []
  readsPrec p (x:y:xs)
    | x ==' ' = readsPrec p (y:xs)
    | Just n <- (x:y:[]) `lookup` s2p = [(n,xs)]
    | otherwise = []
    where s2p = (map swap pairToString)



-- | reverse a vienna pair

revPair :: ViennaPair -> ViennaPair
revPair p
  | p==vpCG = vpGC
  | p==vpGC = vpCG
  | p==vpGU = vpUG
  | p==vpUG = vpGU
  | p==vpAU = vpUA
  | p==vpUA = vpAU
  | p==vpNP = vpNP
  | p==vpNS = vpNS



-- * Convenience structures

cguaP = [vpCG..vpUA]
cgnsP = [vpCG..vpNS]
pairToString = [(vpCG,"CG"),(vpGC,"GC"),(vpUA,"UA"),(vpAU,"AU"),(vpGU,"GU"),(vpUG,"UG"),(vpNS,"NS"),(vpNP,"NP")]

