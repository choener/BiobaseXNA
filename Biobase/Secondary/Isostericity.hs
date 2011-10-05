{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import qualified Data.Map as M
import Text.CSV

import Biobase.Primary
import Biobase.Secondary



class IsostericityLookup a where
  -- | To which classes does a basepair+type belong
  getClasses :: a -> [String] -- TODO this should return [Class]
  -- | What basepairs+type are in a particular class
  inClass :: String -> [a]

instance IsostericityLookup ExtPair where
  getClasses p@((x,y),t)
    | Just cs <- M.lookup p defaultIsostericityMap
    = cs
    {-
    | cs <- M.lookup ((y,x),t) defaultIsostericityMap
    = cs
    | otherwise = [] -}

defaultIsostericityMap = mkIsostericityMap parsedCSV

-- | mapping of (pair,pairtype) to isostericity class

mkIsostericityMap = M.fromListWith (\x y -> nub $ x++y) . mkIsostericityList

mkIsostericityList gs = concatMap f gs where
  f g = map (\e -> ((let [x,y] = fst e in (mkNuc x, mkNuc y), threeChar bpt), nub $ snd e)) $ map entry xs where --  (bpt,map entry xs) where
    bpt = head $ head g
    xs = tail g
    entry x = (x!!0, takeWhile ((=='I') . head) $ drop 2 x)

-- | 

parsedCSV = filter (not . null) gs where
  gs = map (filter ((""/=).head)) . groupBy (\x y -> ""/= (head y)) $ csv
  Right csv = parseCSV "isostericity/detailed" $ BS.unpack detailedCSV

test = do
  mapM_ (\p -> print p >> putStrLn "") parsedCSV

-- ** Raw embeddings

-- | Raw CSV data

detailedCSV :: ByteString
detailedCSV = $(embedFile "sources/isostericity-detailed.csv")

