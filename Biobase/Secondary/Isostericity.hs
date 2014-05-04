{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Provides detailed information on isostericity of RNA basepairs. All data
-- is extracted from csv files which were created from supplemental files in:
--
-- @
-- Frequency and isostericity of RNA base pairs
-- Jesse Stombaugh, Craig L. Zirbel, Eric Westhof, and Neocles B. Leontis
-- Nucl. Acids Res. (2009)
-- doi:10.1093/nar/gkp011
-- @
--

module Biobase.Secondary.Isostericity where

import           Data.ByteString.Char8 (ByteString)
import           Data.FileEmbed (embedFile)
import           Data.Function (on)
import           Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           Text.CSV

import           Biobase.Primary.Nuc
import           Biobase.Secondary.Basepair



-- | Methods to determine the isostericity classes for a given basepair type,
-- or alternatively which basepair types are in a certain isostericity class.
--
-- TODO This requires a major cleanup: right now we are handling 'String's as
-- class descriptors, but we should really be newtype-wrapping or create enum
-- data constructors.

class IsostericityLookup a where
  -- | To which classes does a basepair+type belong
  getClasses :: a -> [String] -- TODO this should return [Class]
  -- | What basepairs+type are in a particular class
  inClass :: String -> [a]

-- | For extended basepairs, we take the default mapping and go from there.
--
-- TODO inClass missing

instance IsostericityLookup ExtPair where
  getClasses p
    | Just cs <- M.lookup p defaultIsostericityMap
    = cs
    | otherwise = []
  inClass x = map fst . filter ((x `elem`).snd) $ M.assocs defaultIsostericityMap

-- | Normal basepairs are assumed to have cWW basepairing.
--
-- TODO inClass missing

instance IsostericityLookup Pair where
  getClasses p
    | Just cs <- M.lookup (p,cWW) defaultIsostericityMap
    = cs
    | otherwise = []
  inClass x = map (baseP.fst) -- remove extended information
            . filter ((cWW==).baseT.fst) -- keep only cWW pairs (baseT-ype)
            . filter ((x `elem`).snd) -- select based on class
            $ M.assocs defaultIsostericityMap



-- ** default data

-- | The default isostericity mapping.

defaultIsostericityMap = mkIsostericityMap parsedCSV

-- | Mapping of (pair,pairtype) to isostericity class.

mkIsostericityMap = M.fromListWith (\x y -> nub $ x++y) . mkIsostericityList

-- | Process CSV list-of-lists to get the isostericity data.

mkIsostericityList :: [[[String]]] -> [(ExtPair, [String])]
mkIsostericityList gs = nubBy ((==) `on` fst) . concatMap turn . concatMap f $ gs where
  f g = map (\e ->  ( ( let [x,y] = fst e
                        in (charRNA x, charRNA y), threeChar bpt
                      )
                    , nub $ snd e)
            ) $ map entry xs where
    bpt = head $ head g
    xs = tail g
    entry x = (x!!0, map (filter (\z -> not $ z `elem` "()")) . takeWhile ('I' `elem`) . drop 2 $ x)
  turn entry@(((x,y),(wc,tx,ty)), cs) = [entry, (((y,x),(wc,ty,tx)), cs)]

-- | Simple parsing of raw CSV data.

parsedCSV = filter (not . null) gs where
  gs = map (filter ((""/=).head)) . groupBy (\x y -> ""/= (head y)) $ csv
  Right csv = parseCSV "isostericity/detailed" $ BS.unpack detailedCSV



-- ** Raw embeddings

-- | Raw CSV data, embedded into the library.

detailedCSV :: ByteString
detailedCSV = $(embedFile "sources/isostericity-detailed.csv")

