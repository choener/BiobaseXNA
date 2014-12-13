
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Convert between different nucleotide representations

module Biobase.Primary.Nuc.Conversion where

import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.Letter (Letter(..), Primary)
import qualified Biobase.Primary.Nuc.DNA as D
import qualified Biobase.Primary.Nuc.RNA as R
import qualified Biobase.Primary.Nuc.XNA as X



-- * Single-character translations.

-- | Transform RNA to DNA. That means change @U@ to @T@ and keep the other
-- characters as is.

rnaTdna = \case
  R.A -> D.A
  R.C -> D.C
  R.G -> D.G
  R.U -> D.T
  _   -> D.N
{-# INLINE rnaTdna #-}

-- | Transform DNA to RNA. That means change @T@ to @U@ and keep the other
-- characters as is.

dnaTrna = \case
  D.A -> R.A
  D.C -> R.C
  D.G -> R.G
  D.T -> R.U
  _   -> R.N
{-# INLINE dnaTrna #-}

-- | Generalize an RNA character to a XNA character.

rnaGxna = \case
  R.A -> X.A
  R.C -> X.C
  R.G -> X.G
  R.U -> X.U
  _   -> X.N
{-# INLINE rnaGxna #-}

-- | Generalize a DNA character to a XNA character.

dnaGxna = \case
  D.A -> X.A
  D.C -> X.C
  D.G -> X.G
  D.T -> X.T
  _   -> X.N
{-# INLINE dnaGxna #-}

-- | Specialize XNA to RNA, @T@ becomes @N@.

xnaSrna = \case
  X.A -> R.A
  X.C -> R.C
  X.G -> R.G
  X.U -> R.U
  _   -> R.N
{-# INLINE xnaSrna #-}

-- | Specialize XNA to DNA, @U@ becomes @N@.

xnaSdna = \case
  X.A -> D.A
  X.C -> D.C
  X.G -> D.G
  X.T -> D.T
  _   -> D.N
{-# INLINE xnaSdna #-}



-- * Reverse-complement of characters.

-- | Produce the complement of a RNA or DNA sequence. Does intentionally
-- not work for XNA sequences as it is not possible to uniquely translate
-- @A@ into either @U@ or @T@.

class Complement s t where
    complement :: s -> t

-- | To 'transcribe' a DNA sequence into RNA we reverse the complement of
-- the sequence.

transcribe :: Primary D.DNA -> Primary R.RNA
transcribe = VU.reverse . complement

instance Complement (Letter R.RNA) (Letter R.RNA) where
    complement = \case
      R.A -> R.U
      R.C -> R.G
      R.G -> R.C
      R.U -> R.A
      R.N -> R.N

instance Complement (Letter D.DNA) (Letter D.DNA) where
    complement = \case
      D.A -> D.T
      D.C -> D.G
      D.G -> D.C
      D.T -> D.A
      D.N -> D.N

instance Complement (Letter D.DNA) (Letter R.RNA) where
    complement = \case
      D.A -> R.U
      D.C -> R.G
      D.G -> R.C
      D.T -> R.A
      D.N -> R.N

instance Complement (Letter R.RNA) (Letter D.DNA) where
    complement = \case
      R.A -> D.T
      R.C -> D.G
      R.G -> D.C
      R.U -> D.A
      R.N -> D.N

instance (Complement s t, VU.Unbox s, VU.Unbox t) => Complement (VU.Vector s) (VU.Vector t) where
    complement = VU.map complement

instance (Complement s t, Functor f) => Complement (f s) (f t) where
    complement = fmap complement

