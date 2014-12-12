
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Biobase.Primary.Nuc.DNA where

import           Data.Char (toUpper)
import           Data.Ix (Ix(..))
import           Data.Primitive.Types
import           Data.String
import           Data.Tuple (swap)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import           Control.Category ((>>>))

import           Biobase.Primary.Bounds
import           Biobase.Primary.Letter



-- | DNA nucleotides.

data DNA

pattern A = Letter 0 :: Letter DNA
pattern C = Letter 1 :: Letter DNA
pattern G = Letter 2 :: Letter DNA
pattern T = Letter 3 :: Letter DNA
pattern N = Letter 4 :: Letter DNA

instance Enum (Letter DNA) where
    succ N          = error "succ/N:DNA"
    succ (Letter x) = Letter $ x+1
    pred A          = error "pred/A:DNA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=4 = Letter k
    toEnum k                = error $ "toEnum/Letter DNA " ++ show k
    fromEnum (Letter k) = k

acgt :: [Letter DNA]
acgt = [A .. T]

charDNA = toUpper >>> \case
    'A' -> A
    'C' -> C
    'G' -> G
    'T' -> T
    _   -> N
{-# INLINE charDNA #-}

dnaChar = \case
  A -> 'A'
  C -> 'C'
  G -> 'G'
  T -> 'T'
  N -> 'N'
{-# INLINE dnaChar #-}            

instance Show (Letter DNA) where
    show c = [dnaChar c]

instance Read (Letter DNA) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | otherwise = [(charDNA x, xs)]

dnaSeq :: MkPrimary n DNA => n -> Primary DNA
dnaSeq = primary

instance Bounded (Letter DNA) where
    minBound = A
    maxBound = N

