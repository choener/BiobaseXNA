
-- | A newtype with an attached phenotype which allows us to encode
-- nucleotides and amino acids. Actual seqence-specific functions can be
-- founds in the appropriate modules @AA@ and @Nuc@.

module Biobase.Primary.Letter where

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Binary
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



-- | A 'Letter' together with its phantom type @t@ encodes bio-sequences.
--
-- TODO should get a polykinded phantom-type do make tape confusion less
-- likely.

newtype Letter t = Letter { getLetter :: Int }
                   deriving (Eq,Ord,Generic,Ix,Typeable)

-- | Manual @Data@ instance because @Letter@ should not show its
-- implementation. This also allows for better use of generic programming
-- downstream.

instance (Typeable t, Typeable (Letter t)) ⇒ Data (Letter t) where
  toConstr = mkIntegralConstr letterDataType . getLetter
  gunfold _ z c = case constrRep c of
    (IntConstr x) → z (Letter $ fromIntegral x)
    _ → errorWithoutStackTrace $ "Biobase.Primary.Letter.gunfold: Constructor "
          ++ show c
          ++ " is not of type Letter (using Int-rep)"
  dataTypeOf _ = letterDataType
letterDataType = mkDataType "Biobase.Primary.Letter" [letterConstr]
letterConstr   = mkConstr letterDataType "Letter" [] Prefix

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
  newtype LimitType (Letter l) = LtLetter (Letter l)
  linearIndex _ (Letter i) = i
  {-# Inline linearIndex #-}
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

deriving instance Eq      (LimitType (Letter l))
deriving instance Generic (LimitType (Letter l))
deriving instance (Read (Letter l)) ⇒ Read    (LimitType (Letter l))
deriving instance (Show (Letter l)) ⇒ Show    (LimitType (Letter l))
deriving instance Typeable (LimitType (Letter l))
deriving instance Data (Letter l) ⇒ Data (LimitType (Letter l))

instance IndexStream z => IndexStream (z:.Letter l) where
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

instance IndexStream (Letter l)

