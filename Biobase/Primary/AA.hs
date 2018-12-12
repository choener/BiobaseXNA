
-- | This module has the translation tables for the genetic code.
--
-- In addition, @Any@ is included to denote that any amino acid is ok, and
-- @Unknown@ to denote unknown data.  We do have a symbol 'Undef' for undefined
-- amino acids, which denotes error condition.
--
-- TODO this nomenclature might change!

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
import qualified GHC.Exts as GHC

import           Biobase.Primary.Letter



-- | Amino acid phantom type.

data AA

pattern  Stop   = Letter  0 ∷ Letter AA n
pattern     A   = Letter  1 ∷ Letter AA n
pattern     B   = Letter  2 ∷ Letter AA n
pattern     C   = Letter  3 ∷ Letter AA n
pattern     D   = Letter  4 ∷ Letter AA n
pattern     E   = Letter  5 ∷ Letter AA n
pattern     F   = Letter  6 ∷ Letter AA n
pattern     G   = Letter  7 ∷ Letter AA n
pattern     H   = Letter  8 ∷ Letter AA n
pattern     I   = Letter  9 ∷ Letter AA n
pattern     K   = Letter 10 ∷ Letter AA n
pattern     L   = Letter 11 ∷ Letter AA n
pattern     M   = Letter 12 ∷ Letter AA n
pattern     N   = Letter 13 ∷ Letter AA n
pattern     P   = Letter 14 ∷ Letter AA n
pattern     Q   = Letter 15 ∷ Letter AA n
pattern     R   = Letter 16 ∷ Letter AA n
pattern     S   = Letter 17 ∷ Letter AA n
pattern     T   = Letter 18 ∷ Letter AA n
pattern     V   = Letter 19 ∷ Letter AA n
pattern     W   = Letter 20 ∷ Letter AA n
pattern     X   = Letter 21 ∷ Letter AA n
pattern     Y   = Letter 22 ∷ Letter AA n
pattern     Z   = Letter 23 ∷ Letter AA n
pattern Any     = Letter 24 ∷ Letter AA n
pattern Unknown = Letter 25 ∷ Letter AA n
pattern Undef   = Letter 26 ∷ Letter AA n

-- * Creating functions and aa data.

aa ∷ Int → Letter AA n
aa = Letter
{-# Inline aa #-}

aaRange = VU.fromList [Stop .. pred Undef]
{-# NoInline aaRange #-}

instance Bounded (Letter AA n) where
    minBound = Stop
    maxBound = Undef

instance LetterChar AA n where
  letterChar = aaChar
  charLetter = charAA

instance ToJSON (Letter AA n) where
  toJSON = toJSON . letterChar

instance FromJSON (Letter AA n) where
  parseJSON = fmap charLetter . parseJSON

--instance (GHC.IsString f) ⇒ ToJSON (Pretty f (Letter AA)) where
--  toJSON = toJSON . T.pack . map letterChar . GHC.toList . getPretty

-- | Translate 'Char' amino acid representation into efficient 'AA' newtype.

charAA ∷ Char → Letter AA n
charAA = B.findWithDefaultL Undef charBaa
{-# INLINE charAA #-}

-- | 'Char' representation of an 'AA'.

aaChar ∷ Letter AA n → Char
aaChar = B.findWithDefaultR '?' charBaa
{-# INLINE aaChar #-}

-- * lookup tables

charBaa ∷ B.Bimap (B.HashMap Char (Letter AA n)) (B.HashMap (Letter AA n) Char)
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
  , ('?',Unknown)
  ]
{-# NOINLINE charBaa #-}

-- | List of the twenty "default" amino acids. Used, for example, by HMMer.

twentyAA ∷ VU.Vector (Letter AA n)
twentyAA = VU.fromList [ A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y ]
{-# NoInline twentyAA #-}


-- * instances

instance Show (Letter AA n) where
  show n = [aaChar n]

instance Read (Letter AA n) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | aa <- charAA x = [(aa,xs)]
    | otherwise = []

instance Enum (Letter AA n) where
    succ Undef      = error "succ/Undef:AA"
    succ (Letter x) = Letter $ x+1
    pred Stop       = error "pred/Stop:AA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=(getLetter Undef) = Letter k
    toEnum k                               = error $ "toEnum/Letter RNA " ++ show k
    fromEnum (Letter k) = k

instance MkPrimary (VU.Vector Char) AA n where
  primary = VU.map charAA

