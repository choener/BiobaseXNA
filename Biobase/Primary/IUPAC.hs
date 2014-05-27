{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Degenerate base symbol representation. We use the same conventions as in
-- <<https://en.wikipedia.org/wiki/Nucleic_acid_notation>> which ignores
-- @U@racil, except if it stands alone for @Char@ and @XNA@ targets. If the
-- 'Degenerate' target is @RNA@, then we create @U@s instead of @T@s.
--
-- TODO Shall we handle 'Complement' for degenerates?

module Biobase.Primary.IUPAC where

import           Control.Arrow ((***))
import           Data.ByteString.Char8 (ByteString,unpack)
import           Data.Char (toUpper)
import           Data.FileEmbed (embedFile)
import           Data.List (nub,sort)
import           Data.String
import           Data.Tuple (swap)
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc


-- | Allow the full, including degenerates, alphabet.

data DEG

nucDEG :: Int -> Letter DEG
nucDEG = Letter

-- NOTE keep this in sync with @sources/iupac-nucleotides@

(degA:degC:degG:degT:degU:degW:degS:degM:degK:degR:degY:degB:degD:degH:degV:degN:_) = map nucDEG [0..]

instance Bounded (Letter DEG) where
    minBound = degA
    maxBound = degN

instance Enum (Letter DEG) where
    succ x | x==degN = error "succ/Letter DEG"
    succ (Letter x)  = Letter $ x+1
    pred x | x==degA = error "pred/Letter DEG"
    pred (Letter x)  = Letter $ x-1
    toEnum k | k>=0 && k<=15 = Letter k
    toEnum k                 = error $ "toEnum/Letter DEG " ++ show k
    fromEnum (Letter k) = k

charDEG = f . toUpper where
  f x = case x of
    'A' -> degA
    'C' -> degC
    'G' -> degG
    'T' -> degT
    'U' -> degU
    'W' -> degW
    'S' -> degS
    'M' -> degM
    'K' -> degK
    'R' -> degR
    'Y' -> degY
    'B' -> degB
    'D' -> degD
    'H' -> degH
    'V' -> degV
    _   -> degN
{-# INLINE charDEG #-}

degChar x | x==degA = 'A'
          | x==degC = 'C'
          | x==degG = 'G'
          | x==degT = 'T'
          | x==degU = 'U'
          | x==degW = 'W'
          | x==degS = 'S'
          | x==degM = 'M'
          | x==degK = 'K'
          | x==degR = 'R'
          | x==degY = 'Y'
          | x==degB = 'B'
          | x==degD = 'D'
          | x==degH = 'H'
          | x==degV = 'V'
          | x==degN = 'N'
{-# INLINE degChar #-}            

instance Show (Letter DEG) where
    show c = [degChar c]

degSeq :: MkPrimary n DEG => n -> Primary DEG
degSeq = primary

instance MkPrimary (VU.Vector Char) DEG where
    primary = VU.map charDEG

instance IsString [Letter DEG] where
    fromString = map charDEG



-- * Conversions

class Degenerate x where
  fromDegenerate :: Char -> [x]
  toDegenerate   :: [x]  -> Maybe Char

instance Degenerate Char where
  fromDegenerate = maybe [] id . flip lookup iupacXDNAchars
  toDegenerate   = flip lookup (map swap iupacXDNAchars) . nub . sort

instance Degenerate (Letter RNA) where
    fromDegenerate 'T' = []
    fromDegenerate x   = map dnaTrna $ fromDegenerate x
    toDegenerate   xs  | xs == [rU] = Just 'U'
                       | otherwise  = toDegenerate $ map rnaTdna xs

instance Degenerate (Letter DNA) where
    fromDegenerate 'U' = []
    fromDegenerate x   = map charDNA $ fromDegenerate x
    toDegenerate       = toDegenerate . map dnaChar

instance Degenerate (Letter XNA) where
    fromDegenerate = map charXNA . fromDegenerate
    toDegenerate   = toDegenerate . map xnaChar

-- * Raw embeddings

-- | list of characters, using the XNA alphabet, but degenerate chars
-- assume DNA characters.

iupacXDNAchars :: [(Char,String)]
iupacXDNAchars = map (go . words) . lines . unpack $ iupacNucleotides where
  go [[c],cs] = (c,cs)
{-# NOINLINE iupacXDNAchars #-}

-- | Raw iupac data, embedded into the library.

iupacNucleotides :: ByteString
iupacNucleotides = $(embedFile "sources/iupac-nucleotides")

