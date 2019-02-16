
module Biobase.Primary.Nuc.RNA where

import           Control.Category ((>>>))
import           Control.Lens (Iso', iso)
import           Data.Aeson
import           Data.Char (toUpper)
import           Data.Data
import           Data.Ix (Ix(..))
import           Data.Primitive.Types
import           Data.String
import           Data.Tuple (swap)
import           Data.Typeable
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Biobase.Types.BioSequence (RNA)

import           Biobase.Primary.Bounds
import           Biobase.Primary.Letter



pattern A = Letter 0 ∷ Letter RNA n
pattern C = Letter 1 ∷ Letter RNA n
pattern G = Letter 2 ∷ Letter RNA n
pattern U = Letter 3 ∷ Letter RNA n
pattern N = Letter 4 ∷ Letter RNA n

instance Bounded (Letter RNA n) where
    minBound = A
    maxBound = N

instance Enum (Letter RNA n) where
    succ N          = error "succ/N:RNA"
    succ (Letter x) = Letter $ x+1
    pred A          = error "pred/A:RNA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=4 = Letter k
    toEnum k                = error $ "toEnum/Letter RNA " ++ show k
    fromEnum (Letter k) = k

instance LetterChar RNA n where
  letterChar = rnaChar
  charLetter = charRNA

instance ToJSON (Letter RNA n) where
  toJSON = toJSON . letterChar

instance FromJSON (Letter RNA n) where
  parseJSON = fmap charLetter . parseJSON

-- We encode 'Primary RNA' directly as a string.
--
-- TODO we can't anymore, because this is not a newtype, just a type.

--instance ToJSON (Primary RNA) where
--  toJSON = toJSON . VU.toList . VU.map letterChar
--
--instance FromJSON (Primary RNA) where
--  parseJSON = fmap (primary ∷ String → Primary RNA) . parseJSON


acgu ∷ [Letter RNA n]
acgu = [A .. U]

charRNA = toUpper >>> \case
    'A' → A
    'C' → C
    'G' → G
    'U' → U
    _   → N
{-# INLINE charRNA #-}

rnaChar = \case
  A → 'A'
  C → 'C'
  G → 'G'
  U → 'U'
  N → 'N'
{-# INLINE rnaChar #-}            

-- | An isomorphism from 'Char' to 'Letter RNA'. This assumes that the
-- underlying @Char@s actually represent an RNA sequence. This allows typesafe
-- modification of RNA sequences since only @[A,C,G,U,N]@ are allowed.

crna ∷ Iso' Char (Letter RNA n)
crna = iso charRNA rnaChar

instance Show (Letter RNA n) where
    show c = [rnaChar c]

instance Read (Letter RNA n) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | otherwise = [(charRNA x, xs)]

rnaSeq ∷ MkPrimary p RNA n ⇒ p → Primary RNA n
rnaSeq = primary

instance MkPrimary (VU.Vector Char) RNA n where
    primary = VU.map charRNA

instance IsString [Letter RNA n] where
    fromString = map charRNA

