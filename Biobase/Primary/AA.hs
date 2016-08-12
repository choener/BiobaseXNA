
-- | This module has the translation tables for the genetic code. We do
-- have a symbol 'Undef' for undefined amino acids (say because of @N@s in
-- the nucleotide code).

module Biobase.Primary.AA where

import           Control.Arrow ((***),first)
import           Data.Aeson
import           Data.Hashable
import           Data.Ix (Ix(..))
import           Data.Map.Strict (Map)
import           Data.Primitive.Types
import           Data.Tuple (swap)
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import qualified GHC.Exts as GHC
import           GHC.Generics (Generic)
import qualified Data.Bijection.HashMap as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

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

instance LetterChar AA where
  letterChar = aaChar
  charLetter = charAA

instance ToJSON (Letter AA) where
  toJSON = toJSON . letterChar

instance FromJSON (Letter AA) where
  parseJSON = fmap charLetter . parseJSON

--instance (GHC.IsString f) => ToJSON (Pretty f (Letter AA)) where
--  toJSON = toJSON . T.pack . map letterChar . GHC.toList . getPretty

-- | Translate 'Char' amino acid representation into efficient 'AA' newtype.

charAA :: Char -> Letter AA
charAA = B.findWithDefaultL Undef charBaa
{-# INLINE charAA #-}

-- | 'Char' representation of an 'AA'.

aaChar :: Letter AA -> Char
aaChar = B.findWithDefaultR '?' charBaa
{-# INLINE aaChar #-}

-- * lookup tables

charBaa :: B.Bimap (B.HashMap Char (Letter AA)) (B.HashMap (Letter AA) Char)
charBaa = B.fromList
  [ ('*',Stop)
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
{-# NOINLINE charBaa #-}



-- * instances

instance Show (Letter AA) where
  show n = [aaChar n]

instance Read (Letter AA) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | aa <- charAA x = [(aa,xs)]
    | otherwise = []

instance Enum (Letter AA) where
    succ Undef      = error "succ/Undef:AA"
    succ (Letter x) = Letter $ x+1
    pred Stop       = error "pred/Stop:AA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=(getLetter Undef) = Letter k
    toEnum k                               = error $ "toEnum/Letter RNA " ++ show k
    fromEnum (Letter k) = k

instance MkPrimary (VU.Vector Char) AA where
  primary = VU.map charAA

