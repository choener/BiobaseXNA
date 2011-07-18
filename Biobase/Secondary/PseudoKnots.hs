{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

-- | Methods to transform a secondary structure containing pseudoknots into a
-- structure which is pseudoknot-free.
--
-- TODO Until a better name is found, this module is home to functions for
-- "de-pseudoknotting" structures.
--
-- TODO Check if there are corner-cases remaining when considering 2-diagrams.

module Biobase.Secondary.PseudoKnots where

import qualified Data.Vector.Unboxed as VU
import Data.List

import Biobase.Secondary



-- | Try to removed pseudoknots from the "pairlist". This works by counting for
-- each pair, how many pairs are incompatible with it. Then those with most
-- incompatibilities are successively removed. This function might well remove
-- more than necessary!

class RemovePseudoKnots a where
  removeByCounting :: a -> a

-- | Remove pseudoknotted pairs from RNA secondary structures.

instance RemovePseudoKnots (VU.Vector PairIdx) where
  removeByCounting = VU.force . wrapRemove where
    wrapRemove !ps
      | VU.null cnts = ps -- there are no pairs
      | mmx == 0     = ps -- there are no incompatibilities
      | otherwise    = wrapRemove $ VU.take pos ps VU.++ VU.drop (pos+1) ps
      where
        cnts = VU.map incomp ps
        mmx = VU.maximum cnts
        Just pos = VU.elemIndex mmx cnts
        incomp (i,j) = VU.length $ VU.filter (\(k,l) -> i<k&&k<j&&j<l || k<i&&i<l&&l<j) ps

-- | Remove pseudoknotted pairs from extended RNA secondary structures.

instance RemovePseudoKnots (VU.Vector ExtPairIdx) where
  removeByCounting = VU.force . wrapRemove where
    wrapRemove !ps
      | VU.null cnts = ps -- there are no pairs
      | mmx == 0     = ps -- there are no incompatibilities
      | otherwise    = wrapRemove $ VU.take pos ps VU.++ VU.drop (pos+1) ps
      where
        cnts = VU.map incomp ps
        mmx = VU.maximum cnts
        Just pos = VU.elemIndex mmx cnts
        incomp ((i,j),_) = VU.length $ VU.filter (\((k,l),_) -> i<k&&k<j&&j<l || k<i&&i<l&&l<j) ps
