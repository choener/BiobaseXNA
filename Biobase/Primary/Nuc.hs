{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The primary structure: interface to efficient encoding of RNA and DNA
-- sequences. The design aims toward the 'vector' library and repa. In
-- particular, everything is strict; if you want to stream full genomes, use
-- 'text' or lazy 'bytestring's instead and cast to Biobase.Primary definitions
-- only at the last moment.
--
-- Degenerate encoding can be found in the @IUPAC@ module.
--
-- TODO enable OverloadedLists

module Biobase.Primary.Nuc
  ( module Biobase.Primary.Letter
  , module Biobase.Primary.Nuc.Conversion
  , module Biobase.Primary.Nuc.DNA
  , module Biobase.Primary.Nuc.RNA
  , module Biobase.Primary.Nuc.XNA
  ) where

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.Conversion
import           Biobase.Primary.Nuc.DNA hiding (A,C,G,T,N)
import           Biobase.Primary.Nuc.RNA hiding (A,C,G,U,N)
import           Biobase.Primary.Nuc.XNA hiding (A,C,G,T,U,N)
import qualified Biobase.Primary.Nuc.DNA as D
import qualified Biobase.Primary.Nuc.RNA as R
import qualified Biobase.Primary.Nuc.XNA as X

