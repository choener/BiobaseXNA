
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
import           Control.Category ((>>>))

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc
import qualified Biobase.Primary.Nuc.RNA as R


-- | Allow the full, including degenerates, alphabet.

data DEG

pattern A = Letter  0 :: Letter DEG n
pattern C = Letter  1 :: Letter DEG n
pattern G = Letter  2 :: Letter DEG n
pattern T = Letter  3 :: Letter DEG n
pattern U = Letter  4 :: Letter DEG n
pattern W = Letter  5 :: Letter DEG n
pattern S = Letter  6 :: Letter DEG n
pattern M = Letter  7 :: Letter DEG n
pattern K = Letter  8 :: Letter DEG n
pattern R = Letter  9 :: Letter DEG n
pattern Y = Letter 10 :: Letter DEG n
pattern B = Letter 11 :: Letter DEG n
pattern D = Letter 12 :: Letter DEG n
pattern H = Letter 13 :: Letter DEG n
pattern V = Letter 14 :: Letter DEG n
pattern N = Letter 15 :: Letter DEG n

instance Bounded (Letter DEG n) where
    minBound = A
    maxBound = N

instance Enum (Letter DEG n) where
    succ N           = error "succ/N:DEG"
    succ (Letter x)  = Letter $ x+1
    pred A           = error "pred/A:DEG"
    pred (Letter x)  = Letter $ x-1
    toEnum k | k>=0 && k<=15 = Letter k
    toEnum k                 = error $ "toEnum/Letter DEG " ++ show k
    fromEnum (Letter k) = k

charDEG = toUpper >>> \case
  'A' -> A
  'C' -> C
  'G' -> G
  'T' -> T
  'U' -> U
  'W' -> W
  'S' -> S
  'M' -> M
  'K' -> K
  'R' -> R
  'Y' -> Y
  'B' -> B
  'D' -> D
  'H' -> H
  'V' -> V
  _   -> N
{-# INLINE charDEG #-}

degChar = \case
  A -> 'A'
  C -> 'C'
  G -> 'G'
  T -> 'T'
  U -> 'U'
  W -> 'W'
  S -> 'S'
  M -> 'M'
  K -> 'K'
  R -> 'R'
  Y -> 'Y'
  B -> 'B'
  D -> 'D'
  H -> 'H'
  V -> 'V'
  N -> 'N'
{-# INLINE degChar #-}            

instance Show (Letter DEG n) where
    show c = [degChar c]

degSeq :: MkPrimary p DEG n => p -> Primary DEG n
degSeq = primary

instance MkPrimary (VU.Vector Char) DEG n where
    primary = VU.map charDEG

instance IsString [Letter DEG n] where
    fromString = map charDEG



-- * Conversions

class Degenerate x where
  fromDegenerate :: Char -> [x]
  toDegenerate   :: [x]  -> Maybe Char

instance Degenerate Char where
  fromDegenerate = maybe [] id . flip lookup iupacXDNAchars
  toDegenerate   = flip lookup (map swap iupacXDNAchars) . nub . sort

instance Degenerate (Letter RNA n) where
    fromDegenerate 'T' = []
    fromDegenerate x   = map dnaTrna $ fromDegenerate x
    toDegenerate   xs  | xs == [R.U] = Just 'U'
                       | otherwise  = toDegenerate $ map rnaTdna xs

instance Degenerate (Letter DNA n) where
    fromDegenerate 'U' = []
    fromDegenerate x   = map charDNA $ fromDegenerate x
    toDegenerate       = toDegenerate . map dnaChar

instance Degenerate (Letter XNA n) where
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

