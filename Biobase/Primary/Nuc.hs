{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | The primary structure: interface to efficient encoding of RNA and DNA
-- sequences. The design aims toward the 'vector' library and repa. In
-- particular, everything is strict; if you want to stream full genomes, use
-- 'text' or lazy 'bytestring's instead and cast to Biobase.Primary definitions
-- only at the last moment.
--
-- NOTE individual nucleotides are encoded is 'Int's internally without any
-- tagging. This means that we have no way of deciding if we are dealing with
-- RNA or DNA on this level.
--
-- TODO enable OverloadedLists
--
-- TODO can we have derivingUnbox for all (Nuc t)?

module Biobase.Primary.Nuc where

import           Data.Char (toUpper)
import           Data.Ix (Ix(..))
import           Data.Primitive.Types
import           Data.String
import           Data.Tuple (swap)
import           Data.Vector.Unboxed.Deriving
import           GHC.Base (remInt,quotInt)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Data.Array.Repa.ExtShape
import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape

import           Biobase.Primary.Bounds



-- * The three different sequence representations. We use the same 'Nuc'
-- representation for all representation, but tag the representations with
-- a phantom type.

data RNA

data DNA

data XNA

newtype Nuc t = Nuc { unNuc :: Int }
                deriving (Eq,Ord,Show)

derivingUnbox "NucRNA"
  [t| Nuc RNA -> Int |] [| unNuc |] [| Nuc |]

derivingUnbox "NucDNA"
  [t| Nuc DNA -> Int |] [| unNuc |] [| Nuc |]

derivingUnbox "NucXNA"
  [t| Nuc XNA -> Int |] [| unNuc |] [| Nuc |]

nucRNA :: Int -> Nuc RNA
nucRNA = Nuc

nucDNA :: Int -> Nuc DNA
nucDNA = Nuc

nucXNA :: Int -> Nuc XNA
nucXNA = Nuc

(rA:rC:rG:rU:rN:_) = map nucRNA [0..]

(dA:dC:dG:dT:dN:_) = map nucDNA [0..]

(xA:xC:xG:xT:xU:xN:_) = map nucXNA [0..]

charRNA = f . toUpper where
  f x = case x of
    'A' -> rA
    'C' -> rC
    'G' -> rG
    'U' -> rU
    _   -> rN
{-# INLINE charRNA #-}

rnaChar x | x==rA = 'A'
          | x==rC = 'C'
          | x==rG = 'G'
          | x==rU = 'U'
          | x==rN = 'N'
{-# INLINE rnaChar #-}            

charDNA = f . toUpper where
  f x = case x of
    'A' -> dA
    'C' -> dC
    'G' -> dG
    'T' -> dT
    _   -> dN
{-# INLINE charDNA #-}

dnaChar x | x==dA = 'A'
          | x==dC = 'C'
          | x==dG = 'G'
          | x==dT = 'T'
          | x==dN = 'N'
{-# INLINE dnaChar #-}            

charXNA = f . toUpper where
  f x = case x of
    'A' -> xA
    'C' -> xC
    'G' -> xG
    'T' -> xT
    'U' -> xU
    _   -> xN
{-# INLINE charXNA #-}

xnaChar x | x==xA = 'A'
          | x==xC = 'C'
          | x==xG = 'G'
          | x==xT = 'T'
          | x==xU = 'U'
          | x==xN = 'N'
{-# INLINE xnaChar #-}            

type Primary t = VU.Vector (Nuc t)

rnaSeq :: MkPrimary n RNA => n -> Primary RNA
rnaSeq = primary

dnaSeq :: MkPrimary n DNA => n -> Primary DNA
dnaSeq = primary

xnaSeq :: MkPrimary n XNA => n -> Primary XNA
xnaSeq = primary

-- | Convert from one representation into the other without the
-- complementary step. As of now, this is only to transform between @U@ and
-- @T@.
--
-- TODO this looks a lot like we could just @unsafeCoerce@ (but lets check
-- if the compiler is smart enough to recognize the pattern)

class Transform s t where
  transform :: s -> t

instance Transform (Nuc DNA) (Nuc RNA) where
    transform z | z==dA     = rA
                | z==dC     = rC
                | z==dG     = rG
                | z==dT     = rU
                | otherwise = rN

instance Transform (Nuc RNA) (Nuc DNA) where
    transform z | z==rA     = dA
                | z==rC     = dC
                | z==rG     = dG
                | z==rU     = dT
                | otherwise = dN

instance (Transform s t, VU.Unbox s, VU.Unbox t) => Transform (VU.Vector s) (VU.Vector t) where
    transform = VU.map transform

instance (Transform s t, Functor f) => Transform (f s) (f t) where
    transform = fmap transform

-- | Produce the complement of a RNA or DNA sequence. Does intentionally
-- not work for XNA sequences as it is not possible to uniquely translate
-- @A@ into either @U@ or @T@.

class Complement s t where
    complement :: s -> t

-- | To 'transcribe' a DNA sequence into RNA we reverse the complement of
-- the sequence.

transcribe :: Primary DNA -> Primary RNA
transcribe = VU.reverse . complement

instance Complement (Nuc RNA) (Nuc RNA) where
    complement z
      | z==rA = rU
      | z==rC = rG
      | z==rG = rC
      | z==rU = rA
      | z==rN = rN

instance Complement (Nuc DNA) (Nuc DNA) where
    complement z
      | z==dA = dT
      | z==dC = dG
      | z==dG = dC
      | z==dT = dA
      | z==dN = dN

instance Complement (Nuc DNA) (Nuc RNA) where
    complement z
      | z==dA = rU
      | z==dC = rG
      | z==dG = rC
      | z==dT = rA
      | z==dN = rN

instance Complement (Nuc RNA) (Nuc DNA) where
    complement z
      | z==rA = dT
      | z==rC = dG
      | z==rG = dC
      | z==rU = dA
      | z==rN = dN

instance (Complement s t, VU.Unbox s, VU.Unbox t) => Complement (VU.Vector s) (VU.Vector t) where
    complement = VU.map complement

instance (Complement s t, Functor f) => Complement (f s) (f t) where
    complement = fmap complement

-- |

{-
transcription :: Primary DNA -> Primary RNA
transcription = VU.map f where
  f z | z==dA = rU
      | z==dC = rG
      | z==dG = rC
      | z==dT = rA
      | z==dN = rN

reverseTranscription :: Primary RNA -> Primary DNA
reverseTranscription 
-}

-- | Conversion from a large number of sequence-like inputs to primary
-- sequences.

class MkPrimary n t where
    primary :: n -> Primary t

instance MkPrimary (VU.Vector Char) RNA where
    primary = VU.map charRNA

instance MkPrimary (VU.Vector Char) DNA where
    primary = VU.map charDNA

instance MkPrimary (VU.Vector Char) XNA where
    primary = VU.map charXNA

instance MkPrimary (VU.Vector Char) t =>  MkPrimary T.Text t where
    primary = primary . VU.fromList . T.unpack

instance MkPrimary (VU.Vector Char) t => MkPrimary TL.Text t where
    primary = primary . VU.fromList . TL.unpack

instance MkPrimary (VU.Vector Char) t => MkPrimary BS.ByteString t where
    primary = primary . VU.fromList . BS.unpack

instance MkPrimary (VU.Vector Char) t => MkPrimary BSL.ByteString t where
    primary = primary . VU.fromList . BSL.unpack



instance IsString [Nuc RNA] where
    fromString = map charRNA

instance IsString [Nuc DNA] where
    fromString = map charDNA

instance IsString [Nuc XNA] where
    fromString = map charXNA

instance (VU.Unbox (Nuc t), IsString [Nuc t]) => IsString (VU.Vector (Nuc t)) where
    fromString = VU.fromList . fromString



instance (Shape sh,Show sh) => Shape (sh :. Nuc z) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.Nuc 0
  unitDim = unitDim:.Nuc 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.Nuc n1) (sh2:.Nuc n2) = addDim sh1 sh2 :. Nuc (n1+n2) -- TODO will not necessarily yield a valid Nuc
  size (sh1:.Nuc n) = size sh1 * n
  sizeIsValid (sh1:.Nuc n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.Nuc sh2) (sh1':.Nuc sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.Nuc d) n = fromIndex ds (n `quotInt` d) :. Nuc r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.Nuc n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. Nuc x
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

instance (Shape sh, Show sh, ExtShape sh) => ExtShape (sh :. Nuc z) where
  subDim (sh1:.Nuc n1) (sh2:.Nuc n2) = subDim sh1 sh2 :. Nuc (n1-n2)
  rangeList (sh1:.Nuc n1) (sh2:.Nuc n2) = [ sh:.Nuc n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2)]]



{-
-- * Convert different types of sequence representations to the internal
-- "Primary Structure" representation

-- | Given a sequence of nucleotides encoded in some "text-form", create a
-- 'Nuc'-based unboxed vector.

class MkPrimary a where
  mkPrimary :: a -> Primary

type Primary = VU.Vector Nuc




-- * Efficient nucleotide encoding

-- A 'Nuc'leotide is simply an Int wrapped up. 'nIMI' provides for
-- intermolecular initialization, 'nN' stands for "any" nucleotides, 'nA',
-- 'nC', 'nG', 'nT' / 'nU' are normal nucleotides.

newtype Nuc = Nuc {unNuc :: Int}
  deriving (Eq,Ord,Ix)

(nN : nA : nC : nG : nT : nU : nIMI : nUndefined : _) = map Nuc [0 .. ]

acgt = [nA,nC,nG,nT]
acgu = [nA,nC,nG,nU]
cgau = [nC,nG,nA,nU]
nacgt = nN:acgt
nacgu = nN:acgu

-- | Translate between 'Char's and 'Nuc's.

mkNuc :: Char -> Nuc
mkNuc = f . toUpper where
  f k
    | Just v <- k `lookup` charNucList = v
    | otherwise = nN

fromNuc :: Nuc -> Char
fromNuc = f where
  f k
    | Just v <- k `lookup` nucCharList = v
    | otherwise = 'N'

charNucList =
  [ ('N',nN)
  , ('A',nA)
  , ('C',nC)
  , ('G',nG)
  , ('T',nT)
  , ('U',nU)
  ]

nucCharList = map swap charNucList

-- ** Methods to convert between DNA and RNA
--
-- TODO add all the rev-comp stuff and whatnot

-- | @T@ to @U@

convT2U x
  | x == nT   = nU
  | otherwise = x

-- | @U@ to @T@

convU2T x
  | x == nU   = nT
  | otherwise = x



-- * Instances of different type classes

-- ** instances for 'Nuc'

-- | Human-readable Show instance.

instance Show Nuc where
  show n = [fromNuc n]

-- | Human-readable Read instance.

instance Read Nuc where
  readsPrec p [] = []
  readsPrec p (x:xs)
    | x ==' ' = readsPrec p xs
    | Just n <- x `lookup` charNucList = [(n,xs)]
    | otherwise = []

derivingUnbox "Nuc"
  [t| Nuc -> Int |] [| unNuc |] [| Nuc |]

-- Shape-based indexing. Nucleotide representations go from nN (0) to nU (4),
-- with additional symbols being available for specialized problems. This is a
-- bit of a problem for shape-based indexing. In particular, we need to be
-- careful with size operations. To include, say, all of nN to nU one needs a
-- size of (z:.nIMI), as nIMI is the first element not in the size anymore.

instance (Shape sh,Show sh) => Shape (sh :. Nuc) where
  rank (sh:._) = rank sh + 1
  zeroDim = zeroDim:.Nuc 0
  unitDim = unitDim:.Nuc 1 -- TODO does this one make sense?
  intersectDim (sh1:.n1) (sh2:.n2) = intersectDim sh1 sh2 :. min n1 n2
  addDim (sh1:.Nuc n1) (sh2:.Nuc n2) = addDim sh1 sh2 :. Nuc (n1+n2) -- TODO will not necessarily yield a valid Nuc
  size (sh1:.Nuc n) = size sh1 * n
  sizeIsValid (sh1:.Nuc n) = sizeIsValid (sh1:.n)
  toIndex (sh1:.Nuc sh2) (sh1':.Nuc sh2') = toIndex (sh1:.sh2) (sh1':.sh2')
  fromIndex (ds:.Nuc d) n = fromIndex ds (n `quotInt` d) :. Nuc r where
                              r | rank ds == 0 = n
                                | otherwise    = n `remInt` d
  inShapeRange (sh1:.n1) (sh2:.n2) (idx:.i) = i>=n1 && i<n2 && inShapeRange sh1 sh2 idx
  listOfShape (sh:.Nuc n) = n : listOfShape sh
  shapeOfList xx = case xx of
    []   -> error "empty list in shapeOfList/Primary"
    x:xs -> shapeOfList xs :. Nuc x
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

instance (Shape sh, Show sh, ExtShape sh) => ExtShape (sh :. Nuc) where
  subDim (sh1:.Nuc n1) (sh2:.Nuc n2) = subDim sh1 sh2 :. Nuc (n1-n2)
  rangeList (sh1:.Nuc n1) (sh2:.Nuc n2) = [ sh:.Nuc n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2)]]

-- | Enum

instance Enum Nuc where
  toEnum = Nuc
  fromEnum = unNuc

-- ** Instances for 'MkPrimary'

instance MkPrimary String where
  mkPrimary = VU.fromList . map mkNuc

instance MkPrimary BS.ByteString where
  mkPrimary = mkPrimary . BS.unpack

instance MkPrimary BSL.ByteString where
  mkPrimary = mkPrimary . BSL.unpack

instance MkPrimary T.Text where
  mkPrimary = mkPrimary . T.unpack

instance MkPrimary [Nuc] where
  mkPrimary = VU.fromList

-}

