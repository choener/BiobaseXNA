
module Biobase.Primary.Nuc.XNA where

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

import           Biobase.Primary.Bounds
import           Biobase.Primary.Letter



-- | Combine both, RNA and DNA.

data XNA

pattern A = Letter 0 :: Letter XNA
pattern C = Letter 1 :: Letter XNA
pattern G = Letter 2 :: Letter XNA
pattern T = Letter 3 :: Letter XNA
pattern U = Letter 4 :: Letter XNA
pattern N = Letter 5 :: Letter XNA

instance Bounded (Letter XNA) where
    minBound = A
    maxBound = N

instance Enum (Letter XNA) where
    succ N          = error "succ/N:XNA"
    succ (Letter x) = Letter $ x+1
    pred A          = error "pred/A:XNA"
    pred (Letter x) = Letter $ x-1
    toEnum k | k>=0 && k<=5 = Letter k
    toEnum k                = error $ "toEnum/Letter XNA " ++ show k
    fromEnum (Letter k) = k

instance LetterChar XNA where
  letterChar = xnaChar
  charLetter = charXNA

--instance (LetterChar XNA) => ToJSON (Primary XNA) where
--  toJSON = toJSON . VU.toList . VU.map letterChar
--
--instance (MkPrimary (VU.Vector Char) XNA) => FromJSON (Primary XNA) where
--  parseJSON = fmap (primary :: String -> Primary XNA) . parseJSON

charXNA = toUpper >>> \case
    'A' -> A
    'C' -> C
    'G' -> G
    'T' -> T
    'U' -> U
    _   -> N
{-# INLINE charXNA #-}

xnaChar = \case
  A -> 'A'
  C -> 'C'
  G -> 'G'
  T -> 'T'
  U -> 'U'
  N -> 'N'
{-# INLINE xnaChar #-}            

instance Show (Letter XNA) where
    show c = [xnaChar c]

instance Read (Letter XNA) where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x==' ' = readsPrec p xs
    | otherwise = [(charXNA x, xs)]

xnaSeq :: MkPrimary n XNA => n -> Primary XNA
xnaSeq = primary

instance MkPrimary (VU.Vector Char) XNA where
    primary = VU.map charXNA

instance IsString [Letter XNA] where
    fromString = map charXNA

