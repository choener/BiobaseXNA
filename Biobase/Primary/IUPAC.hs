{-# LANGUAGE TemplateHaskell #-}

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



class Degenerate x where
  fromSymbol :: x   -> [x]
  toSymbol   :: [x] -> Maybe x

instance Degenerate Char where
  fromSymbol = maybe [] id . flip lookup iupacList
  toSymbol   = flip lookup (map swap iupacList) . nub . sort

-- instance Degenerate RNA where
--
-- instance Degenerate DNA where
--
-- instance Degenerate XNA where -- if we want a combined alphabet



-- ** Raw embeddings

-- | list of characters

iupacList :: [(Char,String)]
iupacList = map (go . words) . lines . unpack $ iupacNucleotides where
  go [[c],cs] = (c,cs)
{-# NOINLINE iupacList #-}

-- | Raw iupac data, embedded into the library.

iupacNucleotides :: ByteString
iupacNucleotides = $(embedFile "sources/iupac-nucleotides")

