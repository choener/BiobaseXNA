
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
import           Data.Map.Strict (Map)
import           Data.Tuple (swap)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

import           Biobase.Types.BioSequence
import           Biobase.Types.Codon

import           Biobase.Primary.AA
import           Biobase.Primary.Nuc
import           Biobase.Primary.Letter
import           Biobase.GeneticCodes.Translation
import           Biobase.GeneticCodes.Types



-- | Transform translation tables into the @Letter DNA/Letter AA@ format.

letterTranslationTable :: TranslationTable Char Char -> TranslationTable (Letter DNA n) (Letter AA n)
letterTranslationTable tbl = TranslationTable
  { _codonToAminoAcid  = M.fromList . map (ftriplet *** felement) . M.toList $ tbl^.codonToAminoAcid
  , _aminoAcidtoCodons = M.fromList . map (charAA *** map felement) . M.toList $ tbl^.aminoAcidtoCodons
  , _tableID           = tbl^.tableID
  , _tableName         = tbl^.tableName
  } where ftriplet :: Codon Char -> Codon (Letter DNA n)
          ftriplet = over each charDNA
          felement :: TranslationElement Char Char -> TranslationElement (Letter DNA n) (Letter AA n)
          felement = over (baseCodon.each) charDNA . over aminoAcid charAA

instance Translation (Codon (Letter DNA n)) where
  type TargetType (Codon (Letter DNA n)) = Letter AA n
  type CodonType (Codon (Letter DNA n)) = Letter DNA n
  type AAType (Codon (Letter DNA n)) = Letter AA n
  translate tbl t = maybe Unknown _aminoAcid $ M.lookup t (tbl^.codonToAminoAcid)
  {-# Inline translate #-}
  translateAllFrames = translate
  {-# Inline translateAllFrames #-}

instance Translation (Primary DNA n) where
  type TargetType (Primary DNA n) = Primary AA n
  type CodonType (Primary DNA n) = Letter DNA n
  type AAType (Primary DNA n) = Letter AA n
  -- |
  --
  -- TODO we could consider returning @Nothing@ in case the input is not
  -- power-of-three.
  translate tbl xs = VU.unfoldrN (VU.length xs `div` 3) go xs
    where go (VU.splitAt 3 -> (hs,ts))
            | VU.length hs < 3 = Nothing
            | otherwise        = Just (aa,ts)
            where [a,b,c] = VU.toList hs
                  aa      = translate tbl $ Codon a b c
  {-# Inline translate #-}
  translateAllFrames tbl xs = VU.unfoldrN (VU.length xs) go 0
    where go 0 = Just (Undef,1)
          go 1 = Just (Undef,2)
          go k = Just (translate tbl $ Codon (xs VU.! (k-2)) (xs VU.! (k-1)) (xs VU.! k), k+1)
  {-# Inlinable translateAllFrames #-}

