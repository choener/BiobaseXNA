{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Encoding of Watson-Crick and Wobble Pairs in the Vienna RNA package style.

module Biobase.Secondary.Vienna where

import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.Ix
import           Data.Primitive.Types
import           Data.Tuple (swap)
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           Prelude as P
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Data.Array.Repa.ExtShape
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc



-- | Use machine Ints internally

newtype ViennaPair = ViennaPair { unViennaPair :: Int }
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

(vpNP:vpCG:vpGC:vpGU:vpUG:vpAU:vpUA:vpNS:vpUndefined:_) = P.map ViennaPair [0..]

class MkViennaPair a where
  mkViennaPair :: a -> ViennaPair
  fromViennaPair :: ViennaPair -> a

instance MkViennaPair (Letter RNA, Letter RNA) where
  mkViennaPair (b1,b2) -- = viennaPairTable `PA.index` (Z:.b1:.b2)
    | b1==rC&&b2==rG = vpCG
    | b1==rG&&b2==rC = vpGC
    | b1==rG&&b2==rU = vpGU
    | b1==rU&&b2==rG = vpUG
    | b1==rA&&b2==rU = vpAU
    | b1==rU&&b2==rA = vpUA
    | otherwise = vpNS
  {-# INLINE mkViennaPair #-}
  fromViennaPair p
    | p==vpCG = (rC,rG)
    | p==vpGC = (rG,rC)
    | p==vpGU = (rG,rU)
    | p==vpUG = (rU,rG)
    | p==vpAU = (rA,rU)
    | p==vpUA = (rU,rA)
    | otherwise = error "non-standard pairs can't be backcasted"
  {-# INLINE fromViennaPair #-}

isViennaPair :: Letter RNA -> Letter RNA -> Bool
isViennaPair a b = f a b where
  f l r =  l==rC && r==rG
        || l==rG && r==rC
        || l==rA && r==rU
        || l==rU && r==rA
        || l==rG && r==rU
        || l==rU && r==rG
  {-# INLINE f #-}
{-# INLINE isViennaPair #-}

viennaPairTable :: Unboxed (Z:.Letter RNA:.Letter RNA) ViennaPair
viennaPairTable = fromAssocs (Z:.rN:.rN) (Z:.rU:.rU) vpNS
  [ (Z:.rC:.rG , vpCG)
  , (Z:.rG:.rC , vpGC)
  , (Z:.rG:.rU , vpGU)
  , (Z:.rU:.rG , vpUG)
  , (Z:.rA:.rU , vpAU)
  , (Z:.rU:.rA , vpUA)
  ]
{-# NOINLINE viennaPairTable #-}

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
    where s2p = (P.map swap pairToString)



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

derivingUnbox "ViennaPair"
  [t| ViennaPair -> Int |] [| unViennaPair |] [| ViennaPair |]

