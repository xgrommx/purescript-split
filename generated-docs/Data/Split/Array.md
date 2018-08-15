## Module Data.Split.Array

#### `splitOn`

``` purescript
splitOn :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

#### `splitOneOf`

``` purescript
splitOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

#### `splitWhen`

``` purescript
splitWhen :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
```

#### `endBy`

``` purescript
endBy :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

#### `endByOneOf`

``` purescript
endByOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

#### `wordsBy`

``` purescript
wordsBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
```

#### `linesBy`

``` purescript
linesBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
```

#### `chunksOf`

``` purescript
chunksOf :: forall e. Int -> Array e -> Array (Array e)
```

#### `splitPlaces`

``` purescript
splitPlaces :: forall e. Array Int -> Array e -> Array (Array e)
```

#### `splitPlacesBlanks`

``` purescript
splitPlacesBlanks :: forall e. Array Int -> Array e -> Array (Array e)
```

#### `chop`

``` purescript
chop :: forall a b. (Array a -> { left :: b, right :: Array a }) -> Array a -> Array b
```

#### `divvy`

``` purescript
divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
```

#### `SplitterArray`

``` purescript
type SplitterArray a = Splitter Array a
```

#### `defaultSplitter`

``` purescript
defaultSplitter :: forall a. SplitterArray a
```

#### `split`

``` purescript
split :: forall a. SplitterArray a -> Array a -> Array (Array a)
```

#### `oneOf`

``` purescript
oneOf :: forall a. Eq a => Array a -> SplitterArray a
```

#### `onSublist`

``` purescript
onSublist :: forall a. Eq a => Array a -> SplitterArray a
```

#### `whenElt`

``` purescript
whenElt :: forall a. (a -> Boolean) -> SplitterArray a
```

#### `dropDelims`

``` purescript
dropDelims :: forall a. SplitterArray a -> SplitterArray a
```

#### `keepDelimsL`

``` purescript
keepDelimsL :: forall a. SplitterArray a -> SplitterArray a
```

#### `keepDelimsR`

``` purescript
keepDelimsR :: forall a. SplitterArray a -> SplitterArray a
```

#### `condense`

``` purescript
condense :: forall a. SplitterArray a -> SplitterArray a
```

#### `dropInitBlank`

``` purescript
dropInitBlank :: forall a. SplitterArray a -> SplitterArray a
```

#### `dropFinalBlank`

``` purescript
dropFinalBlank :: forall a. SplitterArray a -> SplitterArray a
```

#### `dropInnerBlanks`

``` purescript
dropInnerBlanks :: forall a. SplitterArray a -> SplitterArray a
```

#### `dropBlanks`

``` purescript
dropBlanks :: forall a. SplitterArray a -> SplitterArray a
```

#### `startsWith`

``` purescript
startsWith :: forall a. Eq a => Array a -> SplitterArray a
```

#### `startsWithOneOf`

``` purescript
startsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
```

#### `endsWith`

``` purescript
endsWith :: forall a. Eq a => Array a -> SplitterArray a
```

#### `endsWithOneOf`

``` purescript
endsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
```


