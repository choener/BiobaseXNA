
-- | A special class of bounds for RNA/pair encodings that are used to index
-- into tables. We typically encode more in the alphabets than we want to use
-- to index, so in order to keep things simple, we have specialized bounds.

module Biobase.Primary.Bounds where

{-

-- | 'minNormal' and 'maxNormal' encode for, say, ACGU; while 'minExtended' and
-- 'maxExtended' would allow 'N' as well. See Biobase.RNA and
-- Biobase.RNA.ViennaPair for instances.

class Bounded a => Bounds a where
  minNormal :: a
  maxNormal :: a
  minExtended :: a
  maxExtended :: a



-- * Instances for tuples of size 2-6

instance (Bounds a, Bounds b) => Bounds (a,b) where
  minNormal = (minNormal, minNormal)
  maxNormal = (maxNormal, maxNormal)
  minExtended = (minExtended, minExtended)
  maxExtended = (maxExtended, maxExtended)

instance (Bounds a, Bounds b, Bounds c) => Bounds (a,b,c) where
  minNormal = (minNormal, minNormal, minNormal)
  maxNormal = (maxNormal, maxNormal, maxNormal)
  minExtended = (minExtended, minExtended, minExtended)
  maxExtended = (maxExtended, maxExtended, maxExtended)

instance (Bounds a, Bounds b, Bounds c, Bounds d) => Bounds (a,b,c,d) where
  minNormal = (minNormal, minNormal, minNormal, minNormal)
  maxNormal = (maxNormal, maxNormal, maxNormal, maxNormal)
  minExtended = (minExtended, minExtended, minExtended, minExtended)
  maxExtended = (maxExtended, maxExtended, maxExtended, maxExtended)

instance (Bounds a, Bounds b, Bounds c, Bounds d, Bounds e) => Bounds (a,b,c,d,e) where
  minNormal = (minNormal, minNormal, minNormal, minNormal, minNormal)
  maxNormal = (maxNormal, maxNormal, maxNormal, maxNormal, maxNormal)
  minExtended = (minExtended, minExtended, minExtended, minExtended, minExtended)
  maxExtended = (maxExtended, maxExtended, maxExtended, maxExtended, maxExtended)

instance (Bounds a, Bounds b, Bounds c, Bounds d, Bounds e, Bounds f) => Bounds (a,b,c,d,e,f) where
  minNormal = (minNormal, minNormal, minNormal, minNormal, minNormal, minNormal)
  maxNormal = (maxNormal, maxNormal, maxNormal, maxNormal, maxNormal, maxNormal)
  minExtended = (minExtended, minExtended, minExtended, minExtended, minExtended, minExtended)
  maxExtended = (maxExtended, maxExtended, maxExtended, maxExtended, maxExtended, maxExtended)

-}

