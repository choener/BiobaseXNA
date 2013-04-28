{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import System.Console.CmdArgs
import Control.Arrow
import Data.Char (isSpace)
import Data.List
import Data.Ord
import Text.Printf

import Biobase.Secondary.Diagrams
import Biobase.Secondary



data Options = Options
  { structure :: String
  , aq :: Bool
  , qa :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { structure = "" &= args
  , aq = True
  , qa = True
  }

main = do
  Options{..} <- cmdArgs options
  (sqn:xs') <- fmap lines $ getContents
  let xs = map (\(s,e) -> (s,dotBracket ["()"] s,e)) $ map (break isSpace) xs'
  let q = dotBracket ["()"] structure
  let (d,x,e) = getMinimalDistance aq qa q xs
  let distance :: Double = read e - ((/100) . read $ words sqn !! 1)
  printf "%4d %s %s %8.2f\n" d x e distance

getMinimalDistance :: Bool -> Bool -> [PairIdx] -> [(String,[PairIdx],String)] -> (Int,String,String)
getMinimalDistance aq qa q xs = g $ minimumBy (comparing f) xs where
  g (a,b,c) = (f (a,b,c) , a, c)
  f (_,a,_) = length $ aqs ++ qas where
    aqs = if aq then a \\ q else []
    qas = if qa then q \\ a else []
