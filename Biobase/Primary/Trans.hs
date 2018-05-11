
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

import           Control.Lens
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
import           Biobase.GeneticCodes.Translation
import           Biobase.GeneticCodes.Types



-- | Transform translation tables into the @Letter DNA/Letter AA@ format.

letterTranslationTable ∷ TranslationTable Char Char → TranslationTable (Letter DNA) (Letter AA)
letterTranslationTable tbl = TranslationTable
  { _tripletToAminoAcid  = M.fromList . map (ftriplet *** felement) . M.toList $ tbl^.tripletToAminoAcid
  , _aminoAcidtoTriplets = M.fromList . map (charAA *** map felement) . M.toList $ tbl^.aminoAcidtoTriplets
  , _tableID             = tbl^.tableID
  , _tableName           = tbl^.tableName
  } where ftriplet ∷ BaseTriplet Char → BaseTriplet (Letter DNA)
          ftriplet = over tripletChars (map charDNA)
          felement ∷ TranslationElement Char Char → TranslationElement (Letter DNA) (Letter AA)
          felement = over (baseTriplet.tripletChars) (map charDNA) . over aminoAcid charAA

instance Translation (BaseTriplet (Letter DNA)) where
  type TargetType (BaseTriplet (Letter DNA)) = Letter AA
  type TripletType (BaseTriplet (Letter DNA)) = Letter DNA
  type AAType (BaseTriplet (Letter DNA)) = Letter AA
  translate tbl t = maybe Unknown _aminoAcid $ M.lookup t (tbl^.tripletToAminoAcid)
  {-# Inline translate #-}

instance Translation (Primary DNA) where
  type TargetType (Primary DNA) = Primary AA
  type TripletType (Primary DNA) = Letter DNA
  type AAType (Primary DNA) = Letter AA
  -- |
  --
  -- TODO we could consider returning @Nothing@ in case the input is not
  -- power-of-three.
  translate tbl xs = VU.unfoldrN (VU.length xs `div` 3) go xs
    where go (VU.splitAt 3 → (hs,ts))
            | VU.length hs < 3 = Nothing
            | otherwise        = Just (aa,ts)
            where [a,b,c] = VU.toList hs
                  aa      = translate tbl $ BaseTriplet a b c
  {-# Inline translate #-}


{-
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
-}

