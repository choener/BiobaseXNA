
-- | This module provides functionality for translation between nucleotides
-- and amino acids.

module Biobase.Primary.Trans where

{-

-- | Using the codon table, create an 'AAseq' from the 'Primary' sequence.

primaryToAAseq :: Primary -> AAseq
primaryToAAseq = mkAAseq . go where
  go (VU.length -> 0) = []
  go (VU.splitAt 3 -> (VU.toList -> hs,ts)) = case M.lookup hs nucCodonTable of
    Just aa -> aa : go ts
    _       -> error $ "primaryToAAseq: " ++ show (hs,ts)

nucCodonTable = M.fromList . map (map mkNuc *** toAA) . M.assocs $ codonTable

-}

