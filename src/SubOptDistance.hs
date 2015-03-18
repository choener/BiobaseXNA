{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | A very simple program that calculates base pair distances given
-- a secondary structure as argument and a Vienna RNAsubopt output via
-- stdin.

module Main where

import Control.Arrow
import Data.Char (isSpace)
import Data.Either (either)
import Data.Ord
import System.Console.CmdArgs
import Text.Printf

import Biobase.Secondary
import Biobase.Secondary.Diagrams



data Options = Options
  { structure :: String
  , aq :: Bool
  , qa :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { structure = "" &= args
  , aq = True &= help "perform target \\\\ query calculation"
  , qa = True &= help "perform query \\\\ target calculation"
  } &= help helpLines

helpLines = unlines
  [ "This program reads RNAsubopt output on STDIN. Provide an"
  , "additional structure as the argument line. The result will be the"
  , "sub-optimal line with lowest base pair distance to the query line."
  ]

main = do
  Options{..} <- cmdArgs options
  (sqn:xs') <- fmap lines $ getContents
  putStrLn sqn
  let s' = fst . break isSpace $ structure
  let xs = map (viennaStringDistance aq qa s') xs'
  mapM_ (\(s,d) -> printf "%s %6d\n" s d) xs

