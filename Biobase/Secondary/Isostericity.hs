{-# LANGUAGE TemplateHaskell #-}

-- | Provides detailed information on isostericity of RNA basepairs. All data
-- is extracted from csv files which were created from supplemental files in:
--
-- Frequency and isostericity of RNA base pairs
-- Jesse Stombaugh, Craig L. Zirbel, Eric Westhof, and Neocles B. Leontis
-- Nucl. Acids Res. (2009)
-- doi:10.1093/nar/gkp011

module Biobase.Secondary.Isostericity where

import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed (embedFile)
import Data.List
import qualified Data.ByteString.Char8 as BS
import Text.CSV



-- | 

parsedCSV = gs where
  gs = map (filter ((""/=).head)) . groupBy (\x y -> ""/= (head y)) $ csv
  Right csv = parseCSV "isostericity/detailed" $ BS.unpack detailedCSV

test = do
  mapM_ (\p -> print p >> putStrLn "") parsedCSV

-- ** Raw embeddings

-- | Raw CSV data

detailedCSV :: ByteString
detailedCSV = $(embedFile "sources/isostericity-detailed.csv")

