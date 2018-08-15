## Module Data.Common

#### `Delimiter`

``` purescript
newtype Delimiter l a
  = Delimiter (l (a -> Boolean))
```

#### `DelimPolicy`

``` purescript
data DelimPolicy
  = Drop
  | Keep
  | KeepLeft
  | KeepRight
```

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

##### Instances
``` purescript
Generic (Chunk l a) _
(Eq1 l, Eq a) => Eq (Chunk l a)
(Show (l a), Show a) => Show (Chunk l a)
```

#### `Splitter`

``` purescript
type Splitter l a = { delimiter :: Delimiter l a, delimPolicy :: DelimPolicy, condensePolicy :: CondensePolicy, initBlankPolicy :: EndPolicy, finalBlankPolicy :: EndPolicy }
```


