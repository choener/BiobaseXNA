{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Encoding of Watson-Crick and Wobble Pairs in the Vienna RNA package style.

module Biobase.Secondary.Vienna where

import Data.Ix
import Data.Primitive.Types
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Tuple (swap)

import Biobase.Primary
import Biobase.Primary.Bounds



-- | Use machine Ints internally

newtype ViennaPair = ViennaPair Int
  deriving (Eq,Ord,Ix)

(vpNP:vpCG:vpGC:vpGU:vpUG:vpAU:vpUA:vpNS:_) = map ViennaPair [0..]

class MkViennaPair a where
  mkViennaPair :: a -> ViennaPair
  fromViennaPair :: ViennaPair -> a

instance MkViennaPair (Nuc,Nuc) where
  mkViennaPair (b1,b2)
    | b1==nC&&b2==nG = vpCG
    | b1==nG&&b2==nC = vpGC
    | b1==nG&&b2==nU = vpGU
    | b1==nU&&b2==nG = vpUG
    | b1==nA&&b2==nU = vpAU
    | b1==nU&&b2==nA = vpUA
    | otherwise = vpNS
  fromViennaPair p
    | p==vpCG = (nC,nG)
    | p==vpGC = (nG,nC)
    | p==vpGU = (nG,nU)
    | p==vpUG = (nU,nG)
    | p==vpAU = (nA,nU)
    | p==vpUA = (nU,nA)
    | otherwise = error "non-standard pairs can't be backcasted"

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
