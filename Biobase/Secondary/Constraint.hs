{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Biobase.Secondary.Constraint where

import Data.Primitive.Types
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Biobase.Secondary.Diagrams



-- | We can create a constraint from different sources

class MkConstraint a where
  mkConstraint :: a -> Constraint

-- | A constraint is nothing more than a vector of constraint characters
-- together with a possible pairing for each character.

newtype Constraint = Constraint {unConstraint :: VU.Vector (Char,Int)}
  deriving (Show,Read,Eq)

bonusCC = VU.fromList "()<>|"
{-# NOINLINE bonusCC #-}

nobonusCC = VU.fromList ".x"
{-# NOINLINE nobonusCC #-}



-- * Instances

instance MkConstraint String where
  mkConstraint xs = mkConstraint . VU.fromList $ xs

instance MkConstraint (VU.Vector Char) where
  mkConstraint cs = Constraint $ VU.zip cs ks where
    (D1S ks) = mkD1S cs
