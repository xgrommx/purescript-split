## Module Data.Common

#### `Delimiter`

``` purescript
newtype Delimiter l a
  = Delimiter (l (a -> Boolean))
```

A delimiter is a `l` of predicates on elements, matched by some contiguous subsequence of a `l`.

#### `DelimPolicy`

``` purescript
data DelimPolicy
  = Drop
  | Keep
  | KeepLeft
  | KeepRight
```

What to do with delimiters?

* `Drop`	Drop delimiters from the output.
* `Keep`	Keep delimiters as separate chunks of the output.
* `KeepLeft` Keep delimiters in the output, prepending them to the following chunk.
* `KeepRight` Keep delimiters in the output, appending them to the previous chunk.

##### Instances
``` purescript
Generic DelimPolicy _
Eq DelimPolicy
Show DelimPolicy
```

#### `CondensePolicy`

``` purescript
data CondensePolicy
  = Condense
  | DropBlankFields
  | KeepBlankFields
```

What to do with multiple consecutive delimiters?

* `Condens` Condense into a single delimiter.
* `DropBlankFields` Keep consecutive delimiters separate, but don't insert blank chunks in between them.
* `KeepBlankFields` Insert blank chunks between consecutive delimiters.

##### Instances
``` purescript
Generic CondensePolicy _
Eq CondensePolicy
Show CondensePolicy
```

#### `EndPolicy`

``` purescript
data EndPolicy
  = DropBlank
  | KeepBlank
```

What to do with a blank chunk at either end of the `structure` (i.e. when the `structure` begins or ends with a delimiter).

* `DropBlank`
* `KeepBlank`

##### Instances
``` purescript
Generic EndPolicy _
Eq EndPolicy
Show EndPolicy
```

#### `Chunk`

``` purescript
data Chunk l a
  = Delim (l a)
  | Text (l a)
```

Tag chunks as delimiters or text.

* `Delim (l a)`
* `Text (l a)`

##### Instances
``` purescript
Generic (Chunk l a) _
(Eq1 l, Eq a) => Eq (Chunk l a)
(Show (l a), Show a) => Show (Chunk l a)
```

#### `fromElem`

``` purescript
fromElem :: forall l a. Chunk l a -> l a
```

Untag a Chunk.

#### `isDelim`

``` purescript
isDelim :: forall l a. Chunk l a -> Boolean
```

Test whether a Chunk is a delimiter.

#### `isText`

``` purescript
isText :: forall l a. Chunk l a -> Boolean
```

Test whether a Chunk is text.

#### `Splitter`

``` purescript
type Splitter l a = { delimiter :: Delimiter l a, delimPolicy :: DelimPolicy, condensePolicy :: CondensePolicy, initBlankPolicy :: EndPolicy, finalBlankPolicy :: EndPolicy }
```

A splitting strategy for a `l`.

* `delimiter` What delimiter to split on
* `delimPolicy` What to do with delimiters (drop from output, keep as separate elements in output, or merge with previous or following chunks)
* `condensePolicy` What to do with multiple consecutive delimiters
* `initBlankPolicy` Drop an initial blank?
* `finalBlankPolicy` Drop a final blank?


