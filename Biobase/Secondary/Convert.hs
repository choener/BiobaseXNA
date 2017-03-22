
-- | This module gives functionality to convert between different variants
-- of secondary structure elements.

module Biobase.Secondary.Convert where

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Secondary.Basepair
import           Biobase.Secondary.Vienna (ViennaPair(..))
import qualified Biobase.Secondary.Vienna as SV
import qualified Biobase.Secondary.Basepair as SB



-- | @basepairConvert@ converts between different secondary structure base
-- pair representations. In general, the conversion is lossy, in particular
-- when "downsizing", say to @ViennaPair@.

class BasepairConvert a b where
  basepairConvert :: a -> b



-- ** @(RNA,RNA) <-> Basepair@

instance BasepairConvert (Letter RNA,Letter RNA) Basepair where
  basepairConvert (l,r)
    | l >= A && l <= U && r >= A && r <= U
    = BP $ 4 * getLetter l + getLetter r
    | otherwise = NoBP
  {-# Inline basepairConvert #-}

instance BasepairConvert Basepair (Letter RNA, Letter RNA) where
  basepairConvert k
    | k == NoBP || k == NS = (N,N)
    | otherwise = let (l,r) = getBP k `divMod` 4 in (Letter l, Letter r)
  {-# Inline basepairConvert #-}



-- ** @(RNA,RNA) <-> ViennaPair@

instance BasepairConvert (Letter RNA, Letter RNA) ViennaPair where
  basepairConvert = \case
    (C,G) -> SV.CG
    (G,C) -> SV.GC
    (G,U) -> SV.GU
    (U,G) -> SV.UG
    (A,U) -> SV.AU
    (U,A) -> SV.UA
    _     -> SV.NS
  {-# Inline basepairConvert #-}

instance BasepairConvert ViennaPair (Letter RNA, Letter RNA) where
  basepairConvert = \case
    SV.CG -> (C,G)
    SV.GC -> (G,C)
    SV.GU -> (G,U)
    SV.UG -> (U,G)
    SV.AU -> (A,U)
    SV.UA -> (U,A)
    SV.NS -> (N,N)
  {-# Inline basepairConvert #-}



-- ** @Basepair <-> ViennaPair@

instance BasepairConvert Basepair ViennaPair where
  basepairConvert = \case
    SB.AU -> SV.AU
    SB.CG -> SV.CG
    SB.GC -> SV.GC
    SB.GU -> SV.GU
    SB.UA -> SV.UA
    SB.UG -> SV.UG
    _     -> SV.NS
  {-# Inline basepairConvert #-}

instance BasepairConvert ViennaPair Basepair where
  basepairConvert = \case
    SV.AU -> SB.AU
    SV.CG -> SB.CG
    SV.GC -> SB.GC
    SV.GU -> SB.GU
    SV.UA -> SB.UA
    SV.UG -> SB.UG
    _     -> SB.NS
  {-# Inline basepairConvert #-}

