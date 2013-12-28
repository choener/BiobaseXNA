{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module has the translation tables for the genetic code.

module Biobase.AAseq where

import           Control.Arrow ((***))
import           Data.Ix (Ix(..))
import           Data.Primitive.Types
import           Data.Tuple (swap)
import           GHC.Base (remInt,quotInt)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Data.Array.Repa.ExtShape
import Data.Array.Repa.Index
import Data.Array.Repa.Shape

import Biobase.Primary



-- | The amino acid newtype.

newtype AA = AA { unAA :: Int }
  deriving (Eq,Ord,Ix)



-- * Creating functions and aa data.

(aStop:aA:aB:aC:aD:aE:aF:aG:aH:aI:aK:aL:aM:aN:aP:aQ:aR:aS:aT:aV:aW:aX:aY:aZ:aUndefined:_) = map AA [0..]

aaRange = [aStop .. pred aUndefined]

-- | Translate 'Char' amino acid representation into efficient 'AA' newtype.

toAA :: Char -> AA
toAA ((`lookup` charAAList) -> Just aa) = aa
toAA c = error $ "unknown AA: " ++ show c

-- | 'Char' representation of an 'AA'.

fromAA :: AA -> Char
fromAA ((`lookup` aACharList) -> Just c) = c
fromAA (AA aa) = error $ "unknown AA: " ++ (show aa)

-- | Create amino acid sequences from different data sources.

class MkAAseq x where
  mkAAseq :: x -> VU.Vector AA

type AAseq = VU.Vector AA

-- | Using the codon table, create an 'AAseq' from the 'Primary' sequence.

primaryToAAseq :: Primary -> AAseq
primaryToAAseq = mkAAseq . go where
  go (VU.length -> 0) = []
  go (VU.splitAt 3 -> (VU.toList -> hs,ts)) = case M.lookup hs nucCodonTable of
    Just aa -> aa : go ts
    _       -> error $ "primaryToAAseq: " ++ show (hs,ts)



-- * lookup tables

charAAList =
  [ ('/',aStop)
  , ('A',aA)
  , ('B',aB)
  , ('C',aC)
  , ('D',aD)
  , ('E',aE)
  , ('F',aF)
  , ('G',aG)
  , ('H',aH)
  , ('I',aI)
  , ('K',aK)
  , ('L',aL)
  , ('M',aM)
  , ('N',aN)
  , ('P',aP)
  , ('Q',aQ)
  , ('R',aR)
  , ('S',aS)
  , ('T',aT)
  , ('V',aV)
  , ('W',aW)
  , ('X',aX)
  , ('Y',aY)
  , ('Z',aZ)
  ]

aACharList = map swap charAAList

codonTable = M.fromList
  [ ("aaa",'K')
  , ("aac",'N')
  , ("aag",'K')
  , ("aat",'N')
  , ("aca",'T')
  , ("acc",'T')
  , ("acg",'T')
  , ("act",'T')
  , ("aga",'R')
  , ("agc",'S')
  , ("agg",'R')
  , ("agt",'S')
  , ("ata",'I')
  , ("atc",'I')
  , ("atg",'M')
  , ("att",'I')
  , ("caa",'Q')
  , ("cac",'H')
  , ("cag",'Q')
  , ("cat",'H')
  , ("cca",'P')
  , ("ccc",'P')
  , ("ccg",'P')
  , ("cct",'P')
  , ("cga",'R')
  , ("cgc",'R')
  , ("cgg",'R')
  , ("cgt",'R')
  , ("cta",'L')
  , ("ctc",'L')
  , ("ctg",'L')
  , ("ctt",'L')
  , ("gaa",'E')
  , ("gac",'D')
  , ("gag",'E')
  , ("gat",'D')
  , ("gca",'A')
  , ("gcc",'A')
  , ("gcg",'A')
  , ("gct",'A')
  , ("gga",'G')
  , ("ggc",'G')
  , ("ggg",'G')
  , ("ggt",'G')
  , ("gta",'V')
  , ("gtc",'V')
  , ("gtg",'V')
  , ("gtt",'V')
  , ("taa",'/')
  , ("tac",'Y')
  , ("tag",'/')
  , ("tat",'Y')
  , ("tca",'S')
  , ("tcc",'S')
  , ("tcg",'S')
  , ("tct",'S')
  , ("tga",'/')
  , ("tgc",'C')
  , ("tgg",'W')
  , ("tgt",'C')
  , ("tta",'L')
  , ("ttc",'F')
  , ("ttg",'L')
  , ("ttt",'F')
  ]

nucCodonTable = M.fromList . map (map mkNuc *** toAA) . M.assocs $ codonTable



-- * instances

instance Show AA where
  show n = [fromAA n]

instance Read AA where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | Just aa <- x `lookup` charAAList = [(aa,xs)]
    | otherwise = []

deriving instance Prim AA
deriving instance VGM.MVector VU.MVector AA
deriving instance VG.Vector VU.Vector AA
deriving instance VU.Unbox AA

instance (Shape sh,Show sh) => Shape (sh :. AA) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.AA 0
  unitDim = unitDim:.AA 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.AA n1) (sh2:.AA n2) = addDim sh1 sh2 :. AA (n1+n2) -- TODO will not necessarily yield a valid Nuc
  size (sh1:.AA n) = size sh1 * n
  sizeIsValid (sh1:.AA n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.AA sh2) (sh1':.AA sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.AA d) n = fromIndex ds (n `quotInt` d) :. AA r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.AA n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. AA x
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

instance (Shape sh, Show sh, ExtShape sh) => ExtShape (sh :. AA) where
  subDim (sh1:.AA n1) (sh2:.AA n2) = subDim sh1 sh2 :. AA (n1-n2)
  rangeList (sh1:.AA n1) (sh2:.AA n2) = [ sh:.AA n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2)]]

instance Enum AA where
  toEnum = AA
  fromEnum = unAA

instance MkAAseq [Char] where
  mkAAseq = VU.fromList . map toAA

instance MkAAseq [AA] where
  mkAAseq = VU.fromList

instance MkAAseq (VU.Vector Char) where
  mkAAseq = VU.map toAA

instance MkAAseq BS.ByteString where
  mkAAseq = VU.fromList . map toAA . BS.unpack

instance MkAAseq BSL.ByteString where
  mkAAseq = VU.fromList . map toAA . BSL.unpack

instance MkAAseq T.Text where
  mkAAseq = VU.fromList . map toAA . T.unpack

