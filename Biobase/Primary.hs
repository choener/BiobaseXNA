{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | The primary structure: interface to efficient encoding of RNA and DNA
-- sequences. The design aims toward the 'vector' library and repa. In
-- particular, everything is strict; if you want to stream full genomes, use
-- 'text' or lazy 'bytestring's instead and cast to Biobase.Primary definitions
-- only at the last moment.
--
-- NOTE individual nucleotides are encoded is 'Int's internally without any
-- tagging. This means that we have no way of deciding if we are dealing with
-- RNA or DNA on this level.

module Biobase.Primary where

import Data.Char (toUpper)
import Data.Ix (Ix(..))
import Data.Primitive.Types
import Data.Tuple (swap)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Biobase.Primary.Bounds



-- * Convert different types of sequence representations to the internal
-- "Primary Structure" representation

-- | Given a sequence of nucleotides encoded in some "text-form", create a
-- 'Nuc'-based unboxed vector.

class MkPrimary a where
  mkPrimary :: a -> Primary

type Primary = VU.Vector Nuc



-- * Efficient nucleotide encoding

-- A 'Nuc'leotide is simply an Int wrapped up. 'nIMI' provides for
-- intermolecular initialization, 'nN' stands for "any" nucleotides, 'nA',
-- 'nC', 'nG', 'nT' / 'nU' are normal nucleotides.

newtype Nuc = Nuc {unNuc :: Int}
  deriving (Eq,Ord,Ix)

(nN : nA : nC : nG : nT : nIMI : _) = map Nuc [0 .. ]
nU = nT

acgt = [nA..nT]
acgu = acgt
nacgt = [nN..nT]
nacgu = nacgt

-- | Translate between 'Char's and 'Nuc's.

mkNuc :: Char -> Nuc
mkNuc = f . toUpper where
  f k
    | Just v <- k `lookup` charNucList = v
    | otherwise = nN

fromNuc :: Nuc -> Char
fromNuc = f where
  f k
    | Just v <- k `lookup` nucCharList = v
    | otherwise = 'N'

charNucList =
  [ ('N',nN)
  , ('A',nA)
  , ('C',nC)
  , ('G',nG)
  , ('T',nT)
  , ('U',nU)
  ]

nucCharList = map swap charNucList



-- * Instances of different type classes

-- ** instances for 'Nuc'

-- | Human-readable Show instance.

instance Show Nuc where
  show n = [fromNuc n]

-- | Human-readable Read instance.

instance Read Nuc where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x ==' ' = readsPrec p xs
    | Just n <- x `lookup` charNucList = [(n,xs)]
    | otherwise = []

-- for vectors

deriving instance Prim Nuc
deriving instance VGM.MVector VU.MVector Nuc
deriving instance VG.Vector VU.Vector Nuc
deriving instance VU.Unbox Nuc

-- | The bounded instance from GHC proper. Captures all defined symbols.

instance Bounded Nuc where
  minBound = nN
  maxBound = nT

-- | Special bounds for energy / score arrays

instance Bounds Nuc where
  minNormal = nA
  maxNormal = nT
  minExtended = nN
  maxExtended = nT

-- | Enum

instance Enum Nuc where
  toEnum = Nuc
  fromEnum = unNuc

-- ** Instances for 'MkPrimary'

instance MkPrimary String where
  mkPrimary = VU.fromList . map mkNuc

instance MkPrimary BS.ByteString where
  mkPrimary = mkPrimary . BS.unpack

instance MkPrimary BSL.ByteString where
  mkPrimary = mkPrimary . BSL.unpack

instance MkPrimary T.Text where
  mkPrimary = mkPrimary . T.unpack

instance MkPrimary [Nuc] where
  mkPrimary = VU.fromList

