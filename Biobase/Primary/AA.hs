
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module has the translation tables for the genetic code.

module Biobase.Primary.AA where

import           Control.Arrow ((***),first)
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

--import           Data.Array.Repa.ExtShape
--import           Data.Array.Repa.Index
--import           Data.Array.Repa.Shape

import           Biobase.Primary.Letter



-- | Amino acid phantom type.

data AA

pattern  Stop = Letter  0 :: Letter AA
pattern     A = Letter  1 :: Letter AA
pattern     B = Letter  2 :: Letter AA
pattern     C = Letter  3 :: Letter AA
pattern     D = Letter  4 :: Letter AA
pattern     E = Letter  5 :: Letter AA
pattern     F = Letter  6 :: Letter AA
pattern     G = Letter  7 :: Letter AA
pattern     H = Letter  8 :: Letter AA
pattern     I = Letter  9 :: Letter AA
pattern     K = Letter 10 :: Letter AA
pattern     L = Letter 11 :: Letter AA
pattern     M = Letter 12 :: Letter AA
pattern     N = Letter 13 :: Letter AA
pattern     P = Letter 14 :: Letter AA
pattern     Q = Letter 15 :: Letter AA
pattern     R = Letter 16 :: Letter AA
pattern     S = Letter 17 :: Letter AA
pattern     T = Letter 18 :: Letter AA
pattern     V = Letter 19 :: Letter AA
pattern     W = Letter 20 :: Letter AA
pattern     X = Letter 21 :: Letter AA
pattern     Y = Letter 22 :: Letter AA
pattern     Z = Letter 23 :: Letter AA
pattern Undef = Letter 24 :: Letter AA


-- * Creating functions and aa data.

aa :: Int -> Letter AA
aa = Letter

aaRange = [Stop .. pred Undef]

-- | Translate 'Char' amino acid representation into efficient 'AA' newtype.

charAA :: Char -> Letter AA
charAA c
  | c>='/' && c<='Z' = charAaTable `VU.unsafeIndex` i
  where i = fromEnum c
charAA _ = Undef
{-# INLINE charAA #-}

-- | 'Char' representation of an 'AA'.

aaChar :: Letter AA -> Char
aaChar = VU.unsafeIndex aaCharTable . unLetter
{-# INLINE aaChar #-}

-- * lookup tables

charAaTable :: VU.Vector (Letter AA)
charAaTable = VU.replicate (1 + fromEnum 'Z') Undef VU.// xs
  where xs = map (first fromEnum) charAaList
{-# NOINLINE charAaTable #-}

charAaList =
  [ ('/',Stop)
  , ('A',A)
  , ('B',B)
  , ('C',C)
  , ('D',D)
  , ('E',E)
  , ('F',F)
  , ('G',G)
  , ('H',H)
  , ('I',I)
  , ('K',K)
  , ('L',L)
  , ('M',M)
  , ('N',N)
  , ('P',P)
  , ('Q',Q)
  , ('R',R)
  , ('S',S)
  , ('T',T)
  , ('V',V)
  , ('W',W)
  , ('X',X)
  , ('Y',Y)
  , ('Z',Z)
  , ('?',Undef)
  ]
{-# NOINLINE charAaList #-}

aaCharTable :: VU.Vector Char
aaCharTable = VU.fromList $ map (snd . swap) charAaList
{-# NOINLINE aaCharTable #-}



-- * instances

instance Show (Letter AA) where
  show n = [aaChar n]

instance Read (Letter AA) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | aa <- charAA x, aa /= Undef = [(aa,xs)]
    | otherwise = []

instance Enum (Letter AA) where
    succ Undef      = error "succ/Undef:AA"
    succ (Letter x) = Letter $ x+1
    pred Stop       = error "pred/Stop:AA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=(unLetter Undef) = Letter k
    toEnum k                               = error $ "toEnum/Letter RNA " ++ show k
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

