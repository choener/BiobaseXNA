
-- | A newtype with an attached phantom type which allows us to encode
-- nucleotides and amino acids. Actual seqence-specific functions can be founds
-- in the appropriate modules @AA@ and @Nuc@.

module Biobase.Primary.Letter where

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Binary
import           Data.Coerce
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.Ix (Ix(..))
import           Data.Serialize (Serialize(..))
import           Data.String (IsString(..))
import           Data.Typeable
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
import Data.Info



-- | A 'Letter' together with its phantom type @seqTy@ encodes bio-sequences,
-- while @nameTy@ allows to specify a type-level name for a letter.

newtype Letter (seqTy :: *) (nameTy :: k) = Letter { getLetter :: Int }
  deriving (Eq,Ord,Generic,Ix,Typeable)

-- | While @coerce@ will always work, this way restricts the change to just the
-- @nameTy@.

changeNameTy :: Letter seqTy nameTy -> Letter seqTy newNameTy
{-# Inline changeNameTy #-}
changeNameTy = coerce

-- | Manual @Data@ instance because @Letter@ should not show its
-- implementation. This also allows for better use of generic programming
-- downstream.

instance (Typeable t, Typeable (Letter t n)) => Data (Letter t n) where
  toConstr = mkIntegralConstr letterDataType . getLetter
  gunfold _ z c = case constrRep c of
    (IntConstr x) -> z (Letter $ fromIntegral x)
    _ -> errorWithoutStackTrace $ "Biobase.Primary.Letter.gunfold: Constructor "
          ++ show c
          ++ " is not of type Letter (using Int-rep)"
  dataTypeOf _ = letterDataType
letterDataType = mkDataType "Biobase.Primary.Letter" [letterConstr]
letterConstr   = mkConstr letterDataType "Letter" [] Prefix

instance Binary    (Letter t n)
instance Serialize (Letter t n)

instance NFData (Letter t n)

type Primary t n = VU.Vector (Letter t n)

-- | Convert 'Letter' types into character forms. @DNA@, @RNA@, and @amino
-- acid@ sequences can make use of this. Other @Letter@ types only if they
-- have single-char representations.

class LetterChar t n where
  letterChar :: Letter t n -> Char
  charLetter :: Char -> Letter t n

-- | Conversion from a large number of sequence-like inputs to primary
-- sequences.

class MkPrimary c t n where
    primary :: c -> Primary t n

instance MkPrimary (VU.Vector Char) t n => MkPrimary String t n where
    primary = primary . VU.fromList

instance MkPrimary (VU.Vector Char) t n =>  MkPrimary T.Text t n where
    primary = primary . VU.fromList . T.unpack

instance MkPrimary (VU.Vector Char) t n => MkPrimary TL.Text t n where
    primary = primary . VU.fromList . TL.unpack

instance MkPrimary (VU.Vector Char) t n => MkPrimary BS.ByteString t n where
    primary = primary . VU.fromList . BS.unpack

instance MkPrimary (VU.Vector Char) t n => MkPrimary BSL.ByteString t n where
    primary = primary . VU.fromList . BSL.unpack

instance (VU.Unbox (Letter t n), IsString [Letter t n]) => IsString (VU.Vector (Letter t n)) where
    fromString = VU.fromList . fromString



-- *** Instances for 'Letter'.

derivingUnbox "Letter"
  [t| forall t n . Letter t n -> Int |] [| getLetter |] [| Letter |]

instance Hashable (Letter t n)

-- |
--
-- TODO replace @LtLetter Int@ with more specific limits? Maybe some constants?

instance Index (Letter l n) where
  newtype LimitType (Letter l n) = LtLetter (Letter l n)
  linearIndex _ (Letter i) = i
  {-# Inline linearIndex #-}
  fromLinearIndex _ k = Letter k
  {-# Inline fromLinearIndex #-}
  size (LtLetter (Letter h)) = h+1
  {-# Inline size #-}
  inBounds (LtLetter h) i = zeroBound <= i && i <= h
  {-# Inline inBounds #-}
  zeroBound = Letter 0
  {-# Inline zeroBound #-}
  zeroBound' = LtLetter zeroBound
  {-# Inline zeroBound' #-}
  totalSize (LtLetter (Letter k)) = [ fromIntegral k + 1 ]
  {-# Inline totalSize #-}
  showBound (LtLetter (Letter k)) = [ show k ]
  showIndex (Letter k) = [ show k ]

deriving instance Eq      (LimitType (Letter l n))
deriving instance Generic (LimitType (Letter l n))
deriving instance (Read (Letter l n)) => Read    (LimitType (Letter l n))
deriving instance (Show (Letter l n)) => Show    (LimitType (Letter l n))
deriving instance Typeable (LimitType (Letter l n))
deriving instance Data (Letter l n) => Data (LimitType (Letter l n))

instance IndexStream z => IndexStream (z:.Letter l n) where
  streamUp (ls:..LtLetter l) (hs:..LtLetter h) = flatten mk step $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,Letter $ getLetter k +1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:..LtLetter l) (hs:..LtLetter h) = flatten mk step $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ Done
            | otherwise = return $ Yield (z:.k) (z,Letter $ getLetter k -1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream (Letter l n) where
  streamUp l h = map (\(Z:.k) -> k) $ streamUp (ZZ:..l) (ZZ:..h)
  streamDown l h = map (\(Z:.k) -> k) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# Inline streamUp #-}
  {-# Inline streamDown #-}

