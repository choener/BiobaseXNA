{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Arrow
import Data.Char (isSpace)
import Data.Either (either)
import Data.List
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
  [ "This program reads a bunch of RNAsubopt lines on STDIN. Provide an"
  , "additional structure as the argument line. The result will be the"
  , "sub-optimal line with lowest base pair distance to the query line."
  ]

main = do
  Options{..} <- cmdArgs options
  (sqn:xs') <- fmap lines $ getContents
  putStrLn sqn
  let xs = map (extendStructure structure aq qa) xs'
  mapM_ (\(s,d) -> printf "%s %6d\n" s d) xs
  {-
  let xs = map (\(s,e) -> (s,either error id $ dotBracket ["()"] s,read e)) $ map (break isSpace) xs'
  let q = either error id $ dotBracket ["()"] structure
  let (d,x,e) = getMinimalDistance aq qa q xs
  let distance :: Double = e - ((/100) . read $ words sqn !! 1)
  printf "%4d %s %8.2f %8.2f\n" d x e distance
  -}

extendStructure :: String -> Bool -> Bool -> String -> (String,Int)
extendStructure strq aq qa s = (s,length $ aqs++qas) where
  (s',_) = break isSpace s
  t = either error id $ dotBracket ["()"] s'
  q = either error id $ dotBracket ["()"] strq
  aqs = if aq then t \\ q else []
  qas = if qa then q \\ t else []

{-
getMinimalDistance :: Bool -> Bool -> [PairIdx] -> [(String,[PairIdx],Double)] -> (Int,String,Double)
getMinimalDistance aq qa q xs = g $ minimumBy (comparing f) xs where
  g (a,b,c) = ( fst $ f (a,b,c) , a, c)
  f (_,a,e) = (length $ aqs ++ qas , e) where
    aqs = if aq then a \\ q else []
    qas = if qa then q \\ a else []
-}

