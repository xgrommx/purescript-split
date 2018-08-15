## Module Data.Split.List

#### `splitOn`

``` purescript
splitOn :: forall a. Eq a => List a -> List a -> List (List a)
```

#### `splitOneOf`

``` purescript
splitOneOf :: forall a. Eq a => List a -> List a -> List (List a)
```

#### `splitWhen`

``` purescript
splitWhen :: forall a. (a -> Boolean) -> List a -> List (List a)
```

#### `endBy`

``` purescript
endBy :: forall a. Eq a => List a -> List a -> List (List a)
```

#### `endByOneOf`

``` purescript
endByOneOf :: forall a. Eq a => List a -> List a -> List (List a)
```

#### `wordsBy`

``` purescript
wordsBy :: forall a. (a -> Boolean) -> List a -> List (List a)
```

#### `linesBy`

``` purescript
linesBy :: forall a. (a -> Boolean) -> List a -> List (List a)
```

#### `chunksOf`

``` purescript
chunksOf :: forall e. Int -> List e -> List (List e)
```

#### `splitPlaces`

``` purescript
splitPlaces :: forall e. List Int -> List e -> List (List e)
```

#### `splitPlacesBlanks`

``` purescript
splitPlacesBlanks :: forall e. List Int -> List e -> List (List e)
```

#### `chop`

``` purescript
chop :: forall a b. (List a -> { left :: b, right :: List a }) -> List a -> List b
```

#### `divvy`

``` purescript
divvy :: forall a. Int -> Int -> List a -> List (List a)
```

#### `SplitterList`

``` purescript
type SplitterList a = Splitter List a
```

#### `defaultSplitter`

``` purescript
defaultSplitter :: forall a. SplitterList a
```

#### `split`

``` purescript
split :: forall a. SplitterList a -> List a -> List (List a)
```

#### `oneOf`

``` purescript
oneOf :: forall a. Eq a => List a -> SplitterList a
```

#### `onSublist`

``` purescript
onSublist :: forall a. Eq a => List a -> SplitterList a
```

#### `whenElt`

``` purescript
whenElt :: forall a. (a -> Boolean) -> SplitterList a
```

#### `dropDelims`

``` purescript
dropDelims :: forall a. SplitterList a -> SplitterList a
```

#### `keepDelimsL`

``` purescript
keepDelimsL :: forall a. SplitterList a -> SplitterList a
```

#### `keepDelimsR`

``` purescript
keepDelimsR :: forall a. SplitterList a -> SplitterList a
```

#### `condense`

``` purescript
condense :: forall a. SplitterList a -> SplitterList a
```

#### `dropInitBlank`

``` purescript
dropInitBlank :: forall a. SplitterList a -> SplitterList a
```

#### `dropFinalBlank`

``` purescript
dropFinalBlank :: forall a. SplitterList a -> SplitterList a
```

#### `dropInnerBlanks`

``` purescript
dropInnerBlanks :: forall a. SplitterList a -> SplitterList a
```

#### `dropBlanks`

``` purescript
dropBlanks :: forall a. SplitterList a -> SplitterList a
```

#### `startsWith`

``` purescript
startsWith :: forall a. Eq a => List a -> SplitterList a
```

#### `startsWithOneOf`

``` purescript
startsWithOneOf :: forall a. Eq a => List a -> SplitterList a
```

#### `endsWith`

``` purescript
endsWith :: forall a. Eq a => List a -> SplitterList a
```

#### `endsWithOneOf`

``` purescript
endsWithOneOf :: forall a. Eq a => List a -> SplitterList a
```


