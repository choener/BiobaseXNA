
-- | Wrapper newtype to simplify pretty and short encoding of primary
-- sequences.

module Biobase.Primary.Pretty where

import           Data.Aeson
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Text as T

import Biobase.Primary.Letter



newtype Pretty f a = Pretty { getPretty :: f a }

instance (LetterChar x) => ToJSON (Pretty VU.Vector (Letter x)) where
  toJSON = String . T.pack . map letterChar . VU.toList . getPretty

instance (LetterChar x) => ToJSON (Pretty V.Vector (Letter x)) where
  toJSON = String . T.pack . map letterChar . V.toList . getPretty

instance (LetterChar x, VS.Storable (Letter x)) => ToJSON (Pretty VS.Vector (Letter x)) where
  toJSON = String . T.pack . map letterChar . VS.toList . getPretty

instance (LetterChar x) => ToJSON (Pretty [] (Letter x)) where
  toJSON = String . T.pack . map letterChar . getPretty

