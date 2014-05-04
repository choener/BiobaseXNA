{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Fast hash functions for 'Primary' sequences. A hash is just an 'Int', so
-- use these only for short sequences.
--
-- TODO replace with standard hashing functions used by Haskell libs?

module Biobase.Primary.Hashed where

{-

import           Control.Exception.Base (assert)
import           Data.Ix
import           Data.Primitive.Types
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary



newtype HashedPrimary = HashedPrimary { unHashedPrimary :: Int }
  deriving (Eq,Ord,Ix,Read,Show,Enum,Bounded)

derivingUnbox "HashedPrimary"
  [t| HashedPrimary -> Int |] [| unHashedPrimary |] [| HashedPrimary |]


-- | Given a piece of primary sequence information, reduce it to an index.
--
-- Will throw an assertion in debug code if 'ps' are not within bounds. Note
-- that "mkPrimary [minBound]" and "mkPrimary [minBound,minBound]" map to the
-- same index. Meaning that indices are only unique within the same length
-- group. Furthermore, indices with different (l,u)-bounds are not compatible
-- with each other. All indices start at 0.
--
-- The empty input produces an index of 0.
--
-- TODO currently goes the very inefficient way of creating a temporary vector
-- for 'ps'. We could in O(1) create a vector from a Primary ...

mkHashedPrimary :: (Nuc,Nuc) -> Primary -> HashedPrimary
mkHashedPrimary (l,u) ps = assert (VU.all (\p -> l<=p && p<=u) ps) $ HashedPrimary idx where
  idx   = VU.sum $ VU.zipWith f ps (VU.enumFromStepN (VU.length ps -1) (-1) (VU.length ps))
  f p c = (unNuc p - unNuc l) * (cnst^c)
  cnst = unNuc u - unNuc l + 1
{-# INLINE mkHashedPrimary #-}

-}

