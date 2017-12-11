
-- | A newtype with an attached phenotype which allows us to encode
-- nucleotides and amino acids. Actual seqence-specific functions can be
-- founds in the appropriate modules @AA@ and @Nuc@.

module Biobase.Primary.Letter where

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Ix (Ix(..))
import           Data.Serialize (Serialize(..))
import           Data.String (IsString(..))
import           Data.Vector.Fusion.Stream.Monadic (map,Step(..),flatten)
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import           GHC.Generics (Generic)
import           Prelude hiding (map)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Unboxed as VU

import           Data.PrimitiveArray hiding (map)



-- | A 'Letter' together with its phantom type @t@ encodes bio-sequences.

newtype Letter t = Letter { getLetter :: Int }
                   deriving (Eq,Ord,Generic,Ix)

instance Binary    (Letter t)
instance Serialize (Letter t)

instance NFData (Letter t)

type Primary t = VU.Vector (Letter t)

-- | Convert 'Letter' types into character forms. @DNA@, @RNA@, and @amino
-- acid@ sequences can make use of this. Other @Letter@ types only if they
-- have single-char representations.

class LetterChar t where
  letterChar :: Letter t -> Char
  charLetter :: Char -> Letter t

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
  [t| forall a . Letter a -> Int |] [| getLetter |] [| Letter |]

instance Hashable (Letter t)

-- |
--
-- TODO replace @LtLetter Int@ with more specific limits? Maybe some constants?

instance Index (Letter l) where
  newtype LimitType (Letter l) = LtLetter Int
  linearIndex _ (Letter i) = i
  {-# Inline linearIndex #-}
  size (LtLetter h) = h+1
  {-# Inline size #-}
  inBounds (LtLetter h) (Letter i) = 0 <= i && i <= h
  {-# Inline inBounds #-}
  zeroBound = Letter 0
  {-# Inline zeroBound #-}
  zeroBound' = LtLetter 0
  {-# Inline zeroBound' #-}
  sizeIsValid (LtLetter k) = True
  {-# Inline sizeIsValid #-}

instance IndexStream z => IndexStream (z:.Letter l) where
  streamUp (ls:..LtLetter l) (hs:..LtLetter h) = flatten mk step $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ Done
            | otherwise = return $ Yield (z:.Letter k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:..LtLetter l) (hs:..LtLetter h) = flatten mk step $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ Done
            | otherwise = return $ Yield (z:.Letter k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream (Letter l)

