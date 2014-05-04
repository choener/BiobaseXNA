{-# LANGUAGE ViewPatterns #-}

-- | This module provides functionality for translation between nucleotides
-- and amino acids.
--
-- TODO we need different functions, depending on if we have a part of
-- a genome in @DNA@ form, or some messenger @RNA@. It'll probably also be
-- useful to return @Either@, with @Left@ indicating error like partially
-- translated sequence due to intermediate stop codons, or so.

module Biobase.Primary.Trans where

import           Control.Arrow ((***))
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.AA
import           Biobase.Primary.Nuc



-- | Using the codon table, create an 'AAseq' from the 'Primary' sequence.

primaryToAAseq :: Primary DNA -> AAseq
primaryToAAseq = mkAAseq . go where
  go (VU.length -> 0) = []
  go (VU.splitAt 3 -> (hs,ts)) = case M.lookup hs nucCodonTable of
    Just aa -> aa : go ts
    _       -> error $ "primaryToAAseq: " ++ show (hs,ts)

nucCodonTable :: M.Map (Primary DNA) AA
nucCodonTable = M.fromList . map (primary *** toAA) . M.assocs $ codonTable

