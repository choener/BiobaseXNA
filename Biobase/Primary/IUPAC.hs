{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Degenerate base symbol representation. We use the same conventions as in
-- <<https://en.wikipedia.org/wiki/Nucleic_acid_notation>> which ignores
-- @U@racil, except if it stands alone for @Char@ and @XNA@ targets. If the
-- 'Degenerate' target is @RNA@, then we create @U@s instead of @T@s.

module Biobase.Primary.IUPAC where

import Data.ByteString.Char8 (ByteString,unpack)
import Data.FileEmbed (embedFile)
import Data.List (nub,sort)
import Data.Tuple (swap)
import Control.Arrow ((***))

import Biobase.Primary.Letter
import Biobase.Primary.Nuc



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

-- ** Raw embeddings

-- | list of characters, using the XNA alphabet, but degenerate chars
-- assume DNA characters.

iupacXDNAchars :: [(Char,String)]
iupacXDNAchars = map (go . words) . lines . unpack $ iupacNucleotides where
  go [[c],cs] = (c,cs)
{-# NOINLINE iupacXDNAchars #-}

-- | Raw iupac data, embedded into the library.

iupacNucleotides :: ByteString
iupacNucleotides = $(embedFile "sources/iupac-nucleotides")

