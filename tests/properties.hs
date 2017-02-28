
module Main where

import           Control.Monad (join)
import           Data.Bits
import           Data.Function (on)
import           Data.Int (Int16(..))
import           Data.List (groupBy,sort,permutations,nub,(\\))
import           Data.Maybe (isJust)
import           Data.Word (Word)
import           Debug.Trace
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck hiding ((.&.))
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import           Biobase.Secondary.Diagrams
import           Biobase.Secondary.Basepair (PairIdx)



newtype ArbitrarySSTree = ASST (SSTree PairIdx ())
  deriving (Show)

instance Arbitrary ArbitrarySSTree where
  arbitrary = ASST <$> sized arbitrarySSTree
    where
      arbitrarySSTree m = do
        Positive c <- arbitrary
        cs <- go 0 (c*m)
        let k = if null cs then 0 else 1 + maximum [ z | SSTree (_,z) _ _ <- cs ]
        return $ SSExtern k () cs
      go i j = do
        Positive c <- arbitrary
        Positive d <- arbitrary
        if i+c+d >= j
          then return []
          else do
            cs <- go (i+c+1) (i+c+d)
            let h = SSTree (i+c,i+c+d) () cs
            ts <- go (i+c+d+1) j
            return $ h:ts
{-
  shrink (ASST (SSExtern k () cs))
    | null cs = []
    | otherwise = [ ASST
-}

collectPairs (ASST (SSExtern k _ zs)) = (k, sort $ go zs)
  where go [] = []
        go (SSTree (i,j) _ cs : ss) = (i,j) : go cs ++ go ss

bld :: Int -> [PairIdx] -> D1Secondary
bld = curry mkD1S

prop_d1Distance a@(ASST _) b@(ASST _) = d1Distance x y == k
  where x = mkD1S (lx', x')
        y = mkD1S (ly', y')
        (lx', x') = collectPairs a
        (ly', y') = collectPairs b
        k = length $ (x' \\ y') ++ (y' \\ x')

---- | Check if both the memoized version and the population enumeration
---- produce the same multisets, but maybe in different order.
----
---- prop> \(n :: Int16) -> let b = popCount n in memoSorted b == enumSorted b
----
--
--prop_PopCountSet (NonZero (n' :: Int16)) = memo == enum
--  where b    = popCount n
--        memo = memoSorted b
--        enum = enumSorted b
--        n    = n' `mod` 12
--
--memoSorted, enumSorted :: Int -> [[Int]]
--
--memoSorted b = map sort . groupBy ((==) `on` popCount) $ VU.toList $ popCntMemoInt b
--enumSorted b = map sort                                $ [0] : [ roll (popPermutation b) (Just $ 2^k-1) | k <- [1..b] ]
--  where roll f (Just k) = k : roll f (f k)
--        roll _ Nothing  = []
--
--prop_lsb_Int (x :: Int) = lsbZ x == maybe (-1) id (maybeLsb x)
--
--prop_lsb_Word (x :: Word) = lsbZ x == maybe (-1) id (maybeLsb x)
--
--prop_OneBits_Int (x :: Int) = popCount x == length abl && and [ testBit x k | k <- abl ]
--  where abl = activeBitsL x
--
---- Tests if we actually generate all permutations.
--
--prop_allPermutations (a :: Int , b :: Int) = and $ zipWith cmp (sort qs) (sort $ nub ps)
--  where nbs = min a' b' -- number of 1 bits in set
--        sts = max a' b' -- set size
--        a' = a `mod` 8 -- finiteBitSize a
--        b' = b `mod` 8 -- finiteBitSize b
--        ps = permutations $ replicate (sts - nbs) False ++ replicate nbs True
--        qs = go (Just $ 2 ^ nbs - 1)
--        go :: Maybe Int -> [Int]
--        go Nothing  = []
--        go (Just k) = k : go (popPermutation sts k)
--        cmp k as = and [ if a then testBit k c else (not $ testBit k c) | (a,c) <- zip (reverse as) [0 .. ] ]
--
---- TODO popComplement
--
--prop_popShiftL_popShiftR (a::Word,b::Word) = s == l
--  where m = a .|. b
--        s = a .&. b
--        l = popShiftL m r
--        r = popShiftR m s



main :: IO ()
main = $(defaultMainGenerator)

