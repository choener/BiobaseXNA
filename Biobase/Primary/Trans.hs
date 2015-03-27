
-- | This module provides functionality for translation between nucleotides
-- and amino acids.
--
-- NOTE 'aaDNAseq' is lossy. Might be a good idea to consider something
-- more involved?
--
-- TODO we need different functions, depending on if we have a part of
-- a genome in @DNA@ form, or some messenger @RNA@. It'll probably also be
-- useful to return @Either@, with @Left@ indicating error like partially
-- translated sequence due to intermediate stop codons, or so.
--
-- TODO 'dnaAAseq' and 'aaDNAseq' can be nicely optimized using 'flatten'
-- and friends.

module Biobase.Primary.Trans where

import           Control.Arrow ((***))
import           Data.ByteString.Char8 (ByteString,unpack)
import           Data.FileEmbed (embedFile)
import           Data.Map.Strict (Map)
import           Data.Tuple (swap)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.AA
import           Biobase.Primary.Nuc
import           Biobase.Primary.Letter



-- | Using the codon table, create an amino acid sequence from a @DNA@
-- sequence (encoded as 'Primary DNA'). Suffixed @seq@ as we deal with
-- sequences, not letters.

dnaAAseq :: Primary DNA -> Primary AA
dnaAAseq = VU.fromList . go where
  go (VU.length -> 0) = []
  go (VU.splitAt 3 -> (hs,ts)) = case M.lookup hs dnaAAmap of
    Just aa -> aa : go ts
    _       -> error $ "dnaAAseq: " ++ show (hs,ts)

-- | Transform an amino acid sequence back into DNA.
--
-- WARNING: This is lossy!

aaDNAseq :: Primary AA -> Primary DNA
aaDNAseq = VU.concatMap go where
  go aa = case M.lookup aa aaDNAmap of
            Just codon -> codon
            Nothing    -> error $ "aaDNAseq" ++ show aa


-- * Embedded codon data

-- | Lossy backtransformation.

aaDNAmap :: M.Map (Letter AA) (Primary DNA)
aaDNAmap = M.fromList . map swap . M.assocs $ dnaAAmap
{-# NOINLINE aaDNAmap #-}

dnaAAmap :: Map (Primary DNA) (Letter AA)
dnaAAmap = M.fromList . map (primary *** charAA) . M.assocs $ codonTable where
{-# NOINLINE dnaAAmap #-}

codonTable :: Map String Char
codonTable = M.fromList . map (go . words) . lines . unpack $ codonListEmbedded where
  go [cs,[c]] = (cs,c)
  go e        = error $ "codonTable:" ++ show e
{-# NOINLINE codonTable #-}

-- | Raw codon table

codonListEmbedded :: ByteString
codonListEmbedded = $(embedFile "sources/codontable")

