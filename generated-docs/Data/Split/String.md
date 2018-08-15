## Module Data.Split.String

#### `splitOn`

``` purescript
splitOn :: String -> String -> Array String
```

#### `splitOneOf`

``` purescript
splitOneOf :: String -> String -> Array String
```

#### `splitWhen`

``` purescript
splitWhen :: (Char -> Boolean) -> String -> Array String
```

#### `endBy`

``` purescript
endBy :: String -> String -> Array (String)
```

#### `endByOneOf`

``` purescript
endByOneOf :: String -> String -> Array (String)
```

#### `wordsBy`

``` purescript
wordsBy :: (Char -> Boolean) -> String -> Array (String)
```

#### `linesBy`

``` purescript
linesBy :: (Char -> Boolean) -> String -> Array (String)
```

#### `chunksOf`

``` purescript
chunksOf :: Int -> String -> Array String
```

#### `splitPlaces`

``` purescript
splitPlaces :: Array Int -> String -> Array String
```

#### `splitPlacesBlanks`

``` purescript
splitPlacesBlanks :: Array Int -> String -> Array String
```

#### `divvy`

``` purescript
divvy :: Int -> Int -> String -> Array String
```

#### `SplitterChar`

``` purescript
type SplitterChar = SplitterArray Char
```

#### `split`

``` purescript
split :: SplitterChar -> String -> Array String
```

#### `oneOf`

``` purescript
oneOf :: String -> SplitterChar
```

#### `onSublist`

``` purescript
onSublist :: String -> SplitterChar
```

#### `whenElt`

``` purescript
whenElt :: (Char -> Boolean) -> SplitterChar
```

#### `dropDelims`

``` purescript
dropDelims :: SplitterChar -> SplitterChar
```

#### `keepDelimsL`

``` purescript
keepDelimsL :: SplitterChar -> SplitterChar
```

#### `keepDelimsR`

``` purescript
keepDelimsR :: SplitterChar -> SplitterChar
```

#### `condense`

``` purescript
condense :: SplitterChar -> SplitterChar
```

#### `dropInitBlank`

``` purescript
dropInitBlank :: SplitterChar -> SplitterChar
```

#### `dropFinalBlank`

``` purescript
dropFinalBlank :: SplitterChar -> SplitterChar
```

#### `dropInnerBlanks`

``` purescript
dropInnerBlanks :: SplitterChar -> SplitterChar
```

#### `dropBlanks`

``` purescript
dropBlanks :: SplitterChar -> SplitterChar
```

#### `startsWith`

``` purescript
startsWith :: String -> SplitterChar
```

#### `startsWithOneOf`

``` purescript
startsWithOneOf :: String -> SplitterChar
```

#### `endsWith`

``` purescript
endsWith :: String -> SplitterChar
```

#### `endsWithOneOf`

``` purescript
endsWithOneOf :: String -> SplitterChar
```


