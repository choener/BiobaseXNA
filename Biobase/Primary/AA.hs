{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module has the translation tables for the genetic code.

module Biobase.Primary.AA where

import           Control.Arrow ((***))
import           Data.Hashable
import           Data.Ix (Ix(..))
import           Data.Map.Strict (Map)
import           Data.Primitive.Types
import           Data.Tuple (swap)
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape

import           Biobase.Primary.Letter



-- | Amino acid phantom type.

data AA



-- * Creating functions and aa data.

(aStop:aA:aB:aC:aD:aE:aF:aG:aH:aI:aK:aL:aM:aN:aP:aQ:aR:aS:aT:aV:aW:aX:aY:aZ:aUndefined:_) = map aa [0..]

aa :: Int -> Letter AA
aa = Letter

aaRange = [aStop .. pred aUndefined]

-- | Translate 'Char' amino acid representation into efficient 'AA' newtype.

charAA :: Char -> Letter AA
charAA ((`lookup` charAAList) -> Just aa) = aa
charAA c = error $ "unknown AA: " ++ show c

-- | 'Char' representation of an 'AA'.

aaChar :: Letter AA -> Char
aaChar ((`lookup` aaCharList) -> Just c) = c
aaChar (Letter aa) = error $ "unknown AA: " ++ (show aa)



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

aaCharList = map swap charAAList



-- * instances

instance Show (Letter AA) where
  show n = [aaChar n]

instance Read (Letter AA) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | Just aa <- x `lookup` charAAList = [(aa,xs)]
    | otherwise = []

instance Enum (Letter AA) where
    succ x | x==aUndefined = error "succ/Letter RNA"
    succ (Letter x)        = Letter $ x+1
    pred x | x==aStop      = error "pred/Letter RNA"
    pred (Letter x)        = Letter $ x-1
    toEnum k | k>=0 && k<=(unLetter aUndefined) = Letter k
    toEnum k                                    = error $ "toEnum/Letter RNA " ++ show k
    fromEnum (Letter k) = k

instance MkPrimary [Char] AA  where
  primary = VU.fromList . map charAA

instance MkPrimary [Letter AA] AA where
  primary = VU.fromList

instance MkPrimary (VU.Vector Char) AA where
  primary = VU.map charAA

instance MkPrimary BS.ByteString AA where
  primary = VU.fromList . map charAA . BS.unpack

instance MkPrimary BSL.ByteString AA where
  primary = VU.fromList . map charAA . BSL.unpack

instance MkPrimary T.Text AA where
  primary = VU.fromList . map charAA . T.unpack

