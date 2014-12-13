
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Encoding of Watson-Crick and Wobble Pairs in the Vienna RNA package style.

module Biobase.Secondary.Vienna where

import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.Ix
import           Data.Primitive.Types
import           Data.Tuple (swap)
import           Data.Vector.Fusion.Stream.Size (Size (Unknown))
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           Prelude as P
import qualified Data.Vector.Fusion.Stream.Monadic as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Data.Array.Repa.ExtShape
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc
import           Biobase.Primary.Nuc.RNA



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
  rangeStream (fs:.ViennaPair f) (ts:.ViennaPair t) = VM.flatten mk step Unknown $ rangeStream fs ts where
    mk sh = return (sh :. f)
    step (sh :. k)
      | k>t       = return $ VM.Done
      | otherwise = return $ VM.Yield (sh :. ViennaPair k) (sh :. k +1)
    {-# INLINE [1] mk #-}
    {-# INLINE [1] step #-}
  {-# INLINE rangeStream #-}

pattern    NP = ViennaPair 0 :: ViennaPair
pattern    CG = ViennaPair 1 :: ViennaPair
pattern    GC = ViennaPair 2 :: ViennaPair
pattern    GU = ViennaPair 3 :: ViennaPair
pattern    UG = ViennaPair 4 :: ViennaPair
pattern    AU = ViennaPair 5 :: ViennaPair
pattern    UA = ViennaPair 6 :: ViennaPair
pattern    NS = ViennaPair 7 :: ViennaPair
pattern Undef = ViennaPair 8 :: ViennaPair

class MkViennaPair a where
  mkViennaPair :: a -> ViennaPair
  fromViennaPair :: ViennaPair -> a

instance MkViennaPair (Letter RNA, Letter RNA) where
  mkViennaPair = \case
    (C,G) -> CG
    (G,C) -> GC
    (G,U) -> GU
    (U,G) -> UG
    (A,U) -> AU
    (U,A) -> UA
    _     -> NS
  {-# INLINE mkViennaPair #-}
  fromViennaPair = \case
    CG -> (C,G)
    GC -> (G,C)
    GU -> (G,U)
    UG -> (U,G)
    AU -> (A,U)
    UA -> (U,A)
    _  -> error "non-standard pairs can't be backcasted"
  {-# INLINE fromViennaPair #-}

isViennaPair :: Letter RNA -> Letter RNA -> Bool
isViennaPair l r =  l==C && r==G
                 || l==G && r==C
                 || l==A && r==U
                 || l==U && r==A
                 || l==G && r==U
                 || l==U && r==G
{-# INLINE isViennaPair #-}

viennaPairTable :: Unboxed (Z:.Letter RNA:.Letter RNA) ViennaPair
viennaPairTable = fromAssocs (Z:.N:.N) (Z:.U:.U) NS
  [ (Z:.C:.G , CG)
  , (Z:.G:.C , GC)
  , (Z:.G:.U , GU)
  , (Z:.U:.G , UG)
  , (Z:.A:.U , AU)
  , (Z:.U:.A , UA)
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
  minBound = NP
  maxBound = NS

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
revPair = \case
  CG -> GC
  GC -> CG
  GU -> UG
  UG -> GU
  AU -> UA
  UA -> AU
  NP -> NP
  NS -> NS



-- * Convenience structures

cguaP = [CG .. UA]
cgnsP = [CG .. NS]
pairToString = [(CG,"CG"),(GC,"GC"),(UA,"UA"),(AU,"AU"),(GU,"GU"),(UG,"UG"),(NS,"NS"),(NP,"NP")]

derivingUnbox "ViennaPair"
  [t| ViennaPair -> Int |] [| unViennaPair |] [| ViennaPair |]

