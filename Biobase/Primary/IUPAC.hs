{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Degenerate base symbol representation. We use the same conventions as in
-- <<https://en.wikipedia.org/wiki/Nucleic_acid_notation>> which ignores
-- @U@racil, except if it stands alone. Therefore, any RNA sequence should be
-- converted to DNA (and back afterwards).
--
-- NOTE that the generic 'Char' instance is not optimized for speed.
--
-- TODO this should be easier once we have instances for RNA,DNA, etc

module Biobase.Primary.IUPAC where

import Data.ByteString.Char8 (ByteString,unpack)
import Data.FileEmbed (embedFile)
import Data.List (nub,sort)
import Data.Tuple (swap)

import Biobase.Primary.Nuc



class Degenerate x where
  fromSymbol :: x   -> [x]
  toSymbol   :: [x] -> Maybe x

instance Degenerate Char where
  fromSymbol = maybe [] id . flip lookup iupacList
  toSymbol   = flip lookup (map swap iupacList) . nub . sort

-- | TODO use Degenerate DNA and transform 'T' to 'U' (i need a function
-- dna -> rna in Nuc.hs for that)
instance Degenerate (Nuc RNA) where
    fromSymbol = map charRNA . fromSymbol . rnaChar
    toSymbol   = fmap charRNA . toSymbol . map rnaChar

instance Degenerate (Nuc DNA) where
    fromSymbol = map charDNA . fromSymbol . dnaChar
    toSymbol   = fmap charDNA . toSymbol . map dnaChar


-- ** Raw embeddings

-- | list of characters

iupacList :: [(Char,String)]
iupacList = map (go . words) . lines . unpack $ iupacNucleotides where
  go [[c],cs] = (c,cs)
{-# NOINLINE iupacList #-}

-- | Raw iupac data, embedded into the library.

iupacNucleotides :: ByteString
iupacNucleotides = $(embedFile "sources/iupac-nucleotides")

