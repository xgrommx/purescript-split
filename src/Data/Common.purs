module Data.Common where

import Prelude

import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Delimiter l a = Delimiter (l (a -> Boolean))

data DelimPolicy 
  = Drop
  | Keep
  | KeepLeft
  | KeepRight

derive instance genericDelimPolicy :: Generic DelimPolicy _
derive instance eqDelimPolicy :: Eq DelimPolicy

instance showDelimPolicy :: Show DelimPolicy where
  show d = genericShow d

data CondensePolicy 
  = Condense
  | DropBlankFields
  | KeepBlankFields

derive instance genericCondensePolicy :: Generic CondensePolicy _
derive instance eqCondensePolicy :: Eq CondensePolicy

instance showCondensePolicy :: Show CondensePolicy where
  show c = genericShow c

data EndPolicy = DropBlank | KeepBlank

derive instance genericEndPolicy :: Generic EndPolicy _
derive instance eqEndPolicy :: Eq EndPolicy

instance showEndPolicy :: Show EndPolicy where
  show e = genericShow e

data Chunk l a = Delim (l a) | Text (l a)

derive instance genericChunk :: Generic (Chunk l a) _
derive instance eqChunk :: (Eq1 l, Eq a) => Eq (Chunk l a)

instance showChunk :: (Show (l a), Show a) => Show (Chunk l a) where
  show c = genericShow c

type Splitter l a 
  = { delimiter :: Delimiter l a
  , delimPolicy :: DelimPolicy
  , condensePolicy :: CondensePolicy
  , initBlankPolicy :: EndPolicy
  , finalBlankPolicy :: EndPolicy }