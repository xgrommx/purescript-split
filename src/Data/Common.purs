module Data.Common where

import Prelude

import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- | A delimiter is a `l` of predicates on elements, matched by some contiguous subsequence of a `l`.
newtype Delimiter l a = Delimiter (l (a -> Boolean))

-- | What to do with delimiters?
-- |
-- | * `Drop`	Drop delimiters from the output.
-- | * `Keep`	Keep delimiters as separate chunks of the output.
-- | * `KeepLeft` Keep delimiters in the output, prepending them to the following chunk.
-- | * `KeepRight` Keep delimiters in the output, appending them to the previous chunk.
data DelimPolicy 
  = Drop
  | Keep
  | KeepLeft
  | KeepRight

derive instance genericDelimPolicy :: Generic DelimPolicy _
derive instance eqDelimPolicy :: Eq DelimPolicy

instance showDelimPolicy :: Show DelimPolicy where
  show d = genericShow d

-- | What to do with multiple consecutive delimiters?
-- |
-- | * `Condens` Condense into a single delimiter.
-- | * `DropBlankFields` Keep consecutive delimiters separate, but don't insert blank chunks in between them.
-- | * `KeepBlankFields` Insert blank chunks between consecutive delimiters.

data CondensePolicy 
  = Condense
  | DropBlankFields
  | KeepBlankFields

derive instance genericCondensePolicy :: Generic CondensePolicy _
derive instance eqCondensePolicy :: Eq CondensePolicy

instance showCondensePolicy :: Show CondensePolicy where
  show c = genericShow c

-- | What to do with a blank chunk at either end of the `structure` (i.e. when the `structure` begins or ends with a delimiter).
-- |
-- | * `DropBlank`
-- | * `KeepBlank`
data EndPolicy = DropBlank | KeepBlank

derive instance genericEndPolicy :: Generic EndPolicy _
derive instance eqEndPolicy :: Eq EndPolicy

instance showEndPolicy :: Show EndPolicy where
  show e = genericShow e

-- | Tag chunks as delimiters or text.
-- |
-- | * `Delim (l a)`
-- | * `Text (l a)`
data Chunk l a = Delim (l a) | Text (l a)

derive instance genericChunk :: Generic (Chunk l a) _
derive instance eqChunk :: (Eq1 l, Eq a) => Eq (Chunk l a)

instance showChunk :: (Show (l a), Show a) => Show (Chunk l a) where
  show c = genericShow c

-- | A splitting strategy for a `l`.
-- |
-- | * `delimiter` What delimiter to split on
-- | * `delimPolicy` What to do with delimiters (drop from output, keep as separate elements in output, or merge with previous or following chunks)
-- | * `condensePolicy` What to do with multiple consecutive delimiters
-- | * `initBlankPolicy` Drop an initial blank?
-- | * `finalBlankPolicy` Drop a final blank?
type Splitter l a 
  = { delimiter :: Delimiter l a
  , delimPolicy :: DelimPolicy
  , condensePolicy :: CondensePolicy
  , initBlankPolicy :: EndPolicy
  , finalBlankPolicy :: EndPolicy }