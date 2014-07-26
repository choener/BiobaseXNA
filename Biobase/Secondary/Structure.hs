
-- | A secondary structure, with sequence, Vienna compatible canonical
-- secondary structure, extended structure, and additional information.
--
-- This is the structure that will be returned by prediction algorithms in
-- the future.
--
-- TODO we will need ex- and import functions to a number of standard
-- formats. There is an open feature request to export to something that
-- resembles FASTA with additional information.

module Biobase.Secondary.Structure where

import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified Data.Text as T

import           Biobase.Secondary.Diagrams



-- | A sequence, complete with secondary structure. While this structure is
-- rather RNA-centric, there is nothing that prohibits us from using this
-- for DNA.
--
-- TODO Generics, Cereal, Binary, Aeson instances

data SecondaryStructure = SS
  { _ssSeq      :: !Text          -- ^ sequence; we use 'Text' whenever possible
  , _ssVienna   :: !D1Secondary   -- ^ canonical Vienna secondary structure
  , _ssExt      :: !D2Secondary   -- ^ extended secondary structure
  , _ssViennaE  :: Maybe ()       -- ^ TODO will be the energy, measured or predicted
  , _ssAux      :: Map Text Text  -- ^ any auxiliary info in key/value format
  }
  deriving (Eq,Show,Read)

