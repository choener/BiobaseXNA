
-- | New parsers and structures for secondary structures. The structures here a strict.
--
-- TODO Parser should check if a @#Vienna Secondary Structure@ or @#Extended Secondary Structure@ precedes the entries.

module Biobase.Secondary.New where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString,pack)
import Data.Functor
import Data.Tree
import Data.Vector (Vector, fromList)
import GHC.Generics (Generic)



-- | A completely closed sub-structure. An unpaired region @.@ is closed. A
-- paired region @(r)@ is closed, where @r@ contains arbitrarily many unpaired
-- and paired elements.
--
-- TODO Should be extended with @Extended@, but this requires knowing which of
-- the ends overlap with paired: left, right, or both.

data SubStructure (t :: *) a
  = Unpaired { _label :: !a }
  | Paired   { _label :: !a, _subStructures :: !(Vector (SubStructure t a)) }
  deriving (Show, Read, Functor, Traversable, Foldable, Generic, Eq, Ord)
makeLenses ''SubStructure
makePrisms ''SubStructure

-- | A full structure is composed of a number of sub-structures. The empty
-- structure is a full structure.

newtype FullStructure (t :: *) a
  = FullStructure { _fullStructure :: Vector (SubStructure t a) }
  deriving (Show, Read, Functor, Traversable, Foldable, Generic, Eq, Ord)
makeLenses ''FullStructure



-- ** Parses a ViennaRNA secondary structure string.

pUnpaired :: Parser (SubStructure () ())
pUnpaired = Unpaired () <$ char '.'
{-# Inlinable pUnpaired #-}

pPaired :: Parser (SubStructure () ())
pPaired = Paired () <$ char '(' <*> (fromList <$> many pSubStructure) <* char ')'
{-# Inlinable pPaired #-}

pSubStructure :: Parser (SubStructure () ())
pSubStructure = pUnpaired <|> pPaired
{-# Inlinable pSubStructure #-}

pFullStructure :: Parser (FullStructure () ())
pFullStructure = FullStructure <$> fromList <$> many pSubStructure <* endOfInput
{-# Inlinable pFullStructure #-}

newtype StructureParseError = StructureParseError String
  deriving (Show)

parseVienna :: MonadError StructureParseError m ⇒ ByteString -> m (FullStructure () ())
parseVienna = either (throwError . StructureParseError) return . parseOnly pFullStructure
{-# Inlinable parseVienna #-}



-- ** Transform into a @Tree@.

-- | Transform a 'FullStructure' into a 'Tree'.
--
-- Given a full structure generated like this:
-- @
-- s = either (error . show) id $ parseVienna $ pack ".()(())."
-- @
--
-- a tree of just the base paired can be created with
-- @
-- toTree (preview (_Paired._1)) () s
-- @

toTree
  :: (SubStructure t a -> Maybe b)
  -- ^ how to handle substructure elements? @Nothing@ means discard this
  -- substructure and all children.
  -> b
  -- ^ The root label
  -> FullStructure (t :: *) a
  -- ^ The @FullStructure@ to transform into a @Tree@.
  -> Tree b
toTree f r (FullStructure ts) = Node r $ fmap go ts ^.. traverse . _Just
  where
    go u@Unpaired{} = (`Node` []) <$> f u
    go p@Paired{}   = case f p of
      Nothing  -> Nothing
      Just lbl -> Just $ Node lbl $ (fmap go $ p^.subStructures) ^.. traverse . _Just
{-# Inlinable toTree #-}

