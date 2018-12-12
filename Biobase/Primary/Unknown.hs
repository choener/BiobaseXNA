
-- | A 'Letter' with unknown annotation. We sometimes want to encode that
-- we are dealing with @Letter@s in an alphabet, but we do not want to
-- commit to a certain alphabet (just yet).
--
-- This module allows us to make explicit that we do not know the specific
-- alphabet type yet.

module Biobase.Primary.Unknown where

import           Data.Aeson
import           Control.Applicative ((<$>))
import           Control.Arrow ((***),first)
import           Data.Hashable
import           Data.Ix (Ix(..))
import           Data.Map.Strict (Map)
import           Data.Primitive.Types
import           Data.Tuple (swap)
import           Data.Vector.Unboxed.Deriving
import           Debug.Trace
import           GHC.Base (remInt,quotInt)
import           GHC.Generics (Generic)
import           GHC.Read
import qualified Data.Bijection.Map as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Text.ParserCombinators.ReadPrec as RP
import qualified Text.Read.Lex as Lex

import           Biobase.Primary.Letter



-- | @Unknown@ phantom type.

data Unknown



-- | Creating an unknown letter.

unk :: Int -> Letter Unknown n
unk = Letter



-- *** instances

instance Show (Letter Unknown n) where
  show (Letter i) = "U " ++ show i

instance Read (Letter Unknown n) where
  readPrec = parens $ do
    Lex.Ident u <- lexP
    case u of
      "U" -> unk <$> readPrec
      _   -> RP.pfail

instance Enum (Letter Unknown n) where
    succ (Letter x) = Letter $ x+1
    pred (Letter x) = Letter $ x-1
    toEnum = Letter
    fromEnum = getLetter

instance MkPrimary (VU.Vector Int) Unknown n where
  primary = VU.map Letter
  {-# Inline primary #-}

instance ToJSON (Letter Unknown n) where
  toJSON = toJSON . getLetter

instance FromJSON (Letter Unknown n) where
  parseJSON = fmap Letter . parseJSON

{-
instance ToJSON (Primary Unknown) where
  toJSON = toJSON . map (show . getLetter) . VU.toList

instance FromJSON (Primary Unknown) where
  parseJSON = fmap (VU.fromList . map (Letter . read)) . parseJSON
-}

