
-- | Encoding of Watson-Crick and Wobble Pairs in the Vienna RNA package style.

module Biobase.Secondary.Vienna where

import           Data.Aeson
import           Data.Binary
import           Data.Ix
import           Data.Primitive.Types
import           Data.Serialize (Serialize(..))
import           Data.Tuple (swap)
import           Data.Vector.Fusion.Stream.Monadic (map,Step(..),flatten)
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           GHC.Generics (Generic)
import           Prelude hiding (map)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Prelude as P

import           Data.PrimitiveArray hiding (Complement(..),map)
import           Biobase.Types.BioSequence

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc
import           Biobase.Primary.Nuc.RNA



-- | Use machine Ints internally

newtype ViennaPair = ViennaPair { unViennaPair :: Int }
  deriving (Eq,Ord,Generic,Ix)

derivingUnbox "ViennaPair"
  [t| ViennaPair -> Int |]
  [| unViennaPair |]
  [| ViennaPair |]

instance Binary    (ViennaPair)
instance Serialize (ViennaPair)
instance FromJSON  (ViennaPair)
instance ToJSON    (ViennaPair)

instance Index ViennaPair where
  data LimitType ViennaPair
    = Canonical | Extended
  linearIndex _ (ViennaPair p) = p
  {-# Inline linearIndex #-}
  size h = case h of { Canonical → 7; Extended → 9 }
  {-# Inline size #-}
  inBounds h (ViennaPair p) = 0 <= p && p < size h
  {-# Inline inBounds #-}

instance IndexStream z => IndexStream (z:.ViennaPair) where
  streamUp (ls:..l) (hs:..h) = flatten mk step $ streamUp ls hs
    where mk z = return (z,size l - 1)
          step (z,k)
            | k > size h -1 = return $ Done
            | otherwise     = return $ Yield (z:.ViennaPair k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:..l) (hs:..h) = flatten mk step $ streamDown ls hs
    where mk z = return (z,size h - 1)
          step (z,k)
            | k < size l -1 = return $ Done
            | otherwise     = return $ Yield (z:.ViennaPair k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream ViennaPair where



pattern    NP = ViennaPair 0 :: ViennaPair
pattern    CG = ViennaPair 1 :: ViennaPair
pattern    GC = ViennaPair 2 :: ViennaPair
pattern    GU = ViennaPair 3 :: ViennaPair
pattern    UG = ViennaPair 4 :: ViennaPair
pattern    AU = ViennaPair 5 :: ViennaPair
pattern    UA = ViennaPair 6 :: ViennaPair
-- | Non-standard base pair
pattern    NS = ViennaPair 7 :: ViennaPair
pattern Undef = ViennaPair 8 :: ViennaPair

{-
class MkViennaPair a where
  mkViennaPair :: a -> ViennaPair
  fromViennaPair :: ViennaPair -> a

instance MkViennaPair (Letter RNA, Letter RNA) where
  mkViennaPair = \case
    (C,G) -> CG
    (G,C) -> GC
    (G,U) -> GU
    (U,G) -> UG
    (A,U) -> AU
    (U,A) -> UA
    _     -> NS
  {-# INLINE mkViennaPair #-}
  fromViennaPair = \case
    CG -> (C,G)
    GC -> (G,C)
    GU -> (G,U)
    UG -> (U,G)
    AU -> (A,U)
    UA -> (U,A)
    _  -> error "non-standard pairs can't be backcasted"
  {-# INLINE fromViennaPair #-}
-}

isViennaPair :: Letter RNA m -> Letter RNA n -> Bool
isViennaPair l r =  l==C && r==G
                 || l==G && r==C
                 || l==A && r==U
                 || l==U && r==A
                 || l==G && r==U
                 || l==U && r==G
{-# INLINE isViennaPair #-}

viennaPairTable :: Unboxed (Z:.Letter RNA n:.Letter RNA n) ViennaPair
viennaPairTable = fromAssocs (ZZ:..LtLetter maxBound:..LtLetter maxBound) NS
  [ (Z:.C:.G , CG)
  , (Z:.G:.C , GC)
  , (Z:.G:.U , GU)
  , (Z:.U:.G , UG)
  , (Z:.A:.U , AU)
  , (Z:.U:.A , UA)
  ]
{-# NOINLINE viennaPairTable #-}

instance Enum ViennaPair where
  toEnum x
    | x>=0 && x<=7 = ViennaPair x
    | otherwise    = error $ "can't make to enum" ++ show x
  fromEnum (ViennaPair x) = x
  {-# INLINE toEnum #-}
  {-# INLINE fromEnum #-}

instance Bounded ViennaPair where
  minBound = NP
  maxBound = NS

instance Show ViennaPair where
  show x
    | Just s <- x `lookup` pairToString = s
    | otherwise = "??"

instance Read ViennaPair where
  readsPrec p [] = []
  readsPrec p [x] = []
  readsPrec p (x:y:xs)
    | x ==' ' = readsPrec p (y:xs)
    | Just n <- (x:y:[]) `lookup` s2p = [(n,xs)]
    | otherwise = []
    where s2p = (P.map swap pairToString)



-- | reverse a vienna pair

revPair :: ViennaPair -> ViennaPair
revPair = \case
  CG -> GC
  GC -> CG
  GU -> UG
  UG -> GU
  AU -> UA
  UA -> AU
  NP -> NP
  NS -> NS



-- * Convenience structures

cguaP = [CG .. UA]
cgnsP = [CG .. NS]
pairToString = [(CG,"CG"),(GC,"GC"),(UA,"UA"),(AU,"AU"),(GU,"GU"),(UG,"UG"),(NS,"NS"),(NP,"NP")]

