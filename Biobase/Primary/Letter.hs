
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A newtype with an attached phenotype which allows us to encode
-- nucleotides and amino acids. Actual seqence-specific functions can be
-- founds in the appropriate modules @AA@ and @Nuc@.

module Biobase.Primary.Letter where

import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Ix (Ix(..))
import           Data.Serialize
import           Data.String (IsString(..))
import           Data.Vector.Fusion.Stream.Size (Size (Unknown))
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Fusion.Stream.Monadic as VM
import qualified Data.Vector.Unboxed as VU

import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape



-- | A 'Letter' together with its phantom type @t@ encodes bio-sequences.

newtype Letter t = Letter { unLetter :: Int }
                   deriving (Eq,Ord,Generic,Ix)

instance Binary    (Letter t)
instance Serialize (Letter t)
instance FromJSON  (Letter t)
instance ToJSON    (Letter t)

type Primary t = VU.Vector (Letter t)

-- | Conversion from a large number of sequence-like inputs to primary
-- sequences.

class MkPrimary n t where
    primary :: n -> Primary t

instance (MkPrimary (VU.Vector Char) t) => MkPrimary String t where
    primary = primary . VU.fromList

instance MkPrimary (VU.Vector Char) t =>  MkPrimary T.Text t where
    primary = primary . VU.fromList . T.unpack

instance MkPrimary (VU.Vector Char) t => MkPrimary TL.Text t where
    primary = primary . VU.fromList . TL.unpack

instance MkPrimary (VU.Vector Char) t => MkPrimary BS.ByteString t where
    primary = primary . VU.fromList . BS.unpack

instance MkPrimary (VU.Vector Char) t => MkPrimary BSL.ByteString t where
    primary = primary . VU.fromList . BSL.unpack

instance (VU.Unbox (Letter t), IsString [Letter t]) => IsString (VU.Vector (Letter t)) where
    fromString = VU.fromList . fromString



-- *** Instances for 'Letter'.

derivingUnbox "Letter"
  [t| forall a . Letter a -> Int |] [| unLetter |] [| Letter |]

instance Hashable (Letter t)

instance (Shape sh,Show sh) => Shape (sh :. Letter z) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.Letter 0
  unitDim = unitDim:.Letter 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.Letter n1) (sh2:.Letter n2) = addDim sh1 sh2 :. Letter (n1+n2) -- TODO will not necessarily yield a valid Letter
  size (sh1:.Letter n) = size sh1 * n
  sizeIsValid (sh1:.Letter n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.Letter sh2) (sh1':.Letter sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.Letter d) n = fromIndex ds (n `quotInt` d) :. Letter r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.Letter n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. Letter x
  deepSeq (sh:.n) x = deepSeq sh (n `seq` x)
  {-# INLINE rank #-}
  {-# INLINE zeroDim #-}
  {-# INLINE unitDim #-}
  {-# INLINE intersectDim #-}
  {-# INLINE addDim #-}
  {-# INLINE size #-}
  {-# INLINE sizeIsValid #-}
  {-# INLINE toIndex #-}
  {-# INLINE fromIndex #-}
  {-# INLINE inShapeRange #-}
  {-# INLINE listOfShape #-}
  {-# INLINE shapeOfList #-}
  {-# INLINE deepSeq #-}

instance (Shape sh, Show sh, ExtShape sh) => ExtShape (sh :. Letter z) where
  subDim (sh1:.Letter n1) (sh2:.Letter n2) = subDim sh1 sh2 :. Letter (n1-n2)
  rangeList (sh1:.Letter n1) (sh2:.Letter n2) = [ sh:.Letter n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2)]]
  rangeStream (fs:.Letter f) (ts:.Letter t) = VM.flatten mk step Unknown $ rangeStream fs ts where
    mk sh = return (sh :. f)
    step (sh :. k)
      | k>t       = return $ VM.Done
      | otherwise = return $ VM.Yield (sh :. Letter k) (sh :. k +1)
    {-# INLINE [1] mk #-}
    {-# INLINE [1] step #-}
  {-# INLINE rangeStream #-}

