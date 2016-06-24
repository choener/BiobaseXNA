
module Biobase.Primary.Nuc.RNA where

import           Data.Aeson
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
import qualified Data.ByteString.Builder as BB

import           Biobase.Primary.Bounds
import           Biobase.Primary.Letter



-- | RNA nucleotides.

data RNA

pattern A = Letter 0 :: Letter RNA
pattern C = Letter 1 :: Letter RNA
pattern G = Letter 2 :: Letter RNA
pattern U = Letter 3 :: Letter RNA
pattern N = Letter 4 :: Letter RNA

instance Bounded (Letter RNA) where
    minBound = A
    maxBound = N

instance Enum (Letter RNA) where
    succ N          = error "succ/N:RNA"
    succ (Letter x) = Letter $ x+1
    pred A          = error "pred/A:RNA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=4 = Letter k
    toEnum k                = error $ "toEnum/Letter RNA " ++ show k
    fromEnum (Letter k) = k

instance LetterChar RNA where
  letterChar = rnaChar
  charLetter = charRNA

instance ToJSON (Letter RNA) where
  toJSON = toJSON . letterChar

instance FromJSON (Letter RNA) where
  parseJSON = fmap charLetter . parseJSON

-- We encode 'Primary RNA' directly as a string.
--
-- TODO we can't anymore, because this is not a newtype, just a type.

--instance ToJSON (Primary RNA) where
--  toJSON = toJSON . VU.toList . VU.map letterChar
--
--instance FromJSON (Primary RNA) where
--  parseJSON = fmap (primary :: String -> Primary RNA) . parseJSON


acgu :: [Letter RNA]
acgu = [A .. U]

charRNA = toUpper >>> \case
    'A' -> A
    'C' -> C
    'G' -> G
    'U' -> U
    _   -> N
{-# INLINE charRNA #-}

rnaChar = \case
  A -> 'A'
  C -> 'C'
  G -> 'G'
  U -> 'U'
  N -> 'N'
{-# INLINE rnaChar #-}            

instance Show (Letter RNA) where
    show c = [rnaChar c]

instance Read (Letter RNA) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | otherwise = [(charRNA x, xs)]

rnaSeq :: MkPrimary n RNA => n -> Primary RNA
rnaSeq = primary

instance MkPrimary (VU.Vector Char) RNA where
    primary = VU.map charRNA

instance IsString [Letter RNA] where
    fromString = map charRNA

