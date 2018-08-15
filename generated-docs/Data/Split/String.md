## Module Data.Split.String

#### `splitOn`

``` purescript
splitOn :: String -> String -> Array String
```

Split on the given substrings. 
Equivalent to `split <<< dropDelims <<< onSublist`. For example:
```purescript
splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
```

#### `splitOneOf`

``` purescript
splitOneOf :: String -> String -> Array String
```

Split on any of the given elements. 
Equivalent to `split <<< dropDelims <<< oneOf`. For example:
```purescript
splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
```

#### `splitWhen`

``` purescript
splitWhen :: (Char -> Boolean) -> String -> Array String
```

Split on elements satisfying the given predicate. 
Equivalent to `split <<< dropDelims <<< whenElt`. For example:
```purescript
splitWhen (\c -> c == 'a') "asaaxsadsdaxadadaax" == ["","s","","xs","dsd","x","d","d","","x"]
```

#### `endBy`

``` purescript
endBy :: String -> String -> Array (String)
```

Split into chunks terminated by the given substrings. 
Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< onSublist`. For example:
```purescript
endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
```

#### `endByOneOf`

``` purescript
endByOneOf :: String -> String -> Array (String)
```

Split into chunks terminated by one of the given elements. 
Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< oneOf`. For example:
```purescript
endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
```

#### `wordsBy`

``` purescript
wordsBy :: (Char -> Boolean) -> String -> Array (String)
```

Split into "words", with word boundaries indicated by the given predicate. 
Equivalent to `split <<< dropBlanks <<< dropDelims <<< whenElt`. For example:
```purescript
wordsBy (_=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
```

#### `linesBy`

``` purescript
linesBy :: (Char -> Boolean) -> String -> Array (String)
```

Split into "lines", with line boundaries indicated by the given predicate. 
Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< whenElt`. For example:
```purescript
linesBy (_=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
```

#### `chunksOf`

``` purescript
chunksOf :: Int -> String -> Array String
```

`chunksOf` n splits a string into length-n pieces. 
The last piece will be shorter if n does not evenly divide the length of the string. 
This is intentional, and is consistent with a recursive definition of `chunksOf`; it satisfies the property that
```purescript
chunksOf n xs <> chunksOf n ys == chunksOf n (xs <> ys)
```
whenever n evenly divides the length of xs

#### `splitPlaces`

``` purescript
splitPlaces :: Array Int -> String -> Array String
```

Split a string into chunks of the given lengths. For example:
```purescript
splitPlaces [2,3,4] "asaaxsadsdaxadadaax" == ["as","aax","sads"]
```
```purescript
splitPlaces [4,9] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa"]
```
```purescript  
splitPlaces [4,9,3] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa","dad"]
```
If the input string is longer than the total of the given lengths,
then the remaining elements are dropped. If the string is shorter
than the total of the given lengths, then the result may contain
fewer chunks than requested, and the last chunk may be shorter
than requested.

#### `splitPlacesBlanks`

``` purescript
splitPlacesBlanks :: Array Int -> String -> Array String
```

Split a string into chunks of the given lengths. Unlike
`splitPlaces`, the output string will always be the same length as
the first input argument. If the input string is longer than the
total of the given lengths, then the remaining elements are
dropped. If the array is shorter than the total of the given
lengths, then the last several chunks will be shorter than
requested or empty. For example:
```purescript
splitPlacesBlanks [2,3,4] "asaaxsadsdaxadadaax" == ["as","aax","sads"]
```
```purescript  
splitPlacesBlanks [4,9] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa"]
```
```purescript  
splitPlacesBlanks [4,9,3] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa","dad"]
```
Notice the empty array in the output of the third example, which
differs from the behavior of `splitPlaces`.

#### `divvy`

``` purescript
divvy :: Int -> Int -> String -> Array String
```

Divides up an input string into a set of substrings, according to `n` and `m`
input specifications you provide. Each substring will have `n` items, and the
start of each substring will be offset by `m` items from the previous one.
```purescript
divvy 5 5 "asaaxsadsdaxadadaax" == ["asaax","sadsd","axada"]
```
In the case where a source strings's trailing elements do no fill an entire
substring, those trailing elements will be dropped.
```purescript
divvy 5 2 "asaaxsadsdaxadadaax" == ["asaax","aaxsa","xsads","adsda","sdaxa","axada","adada","adaax"]
```

#### `SplitterChar`

``` purescript
type SplitterChar = SplitterArray Char
```

A splitting strategy for an `string`.

#### `split`

``` purescript
split :: SplitterChar -> String -> Array String
```

Split a array according to the given splitting strategy. This is
how to \"run\" a `SplitterChar` that has been built using the other
combinators.

#### `oneOf`

``` purescript
oneOf :: String -> SplitterChar
```

A splitting strategy that splits on any one of the given elements. For example:
```purescript
split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
```

#### `onSublist`

``` purescript
onSublist :: String -> SplitterChar
```

A splitting strategy that splits on the given string, when it is encountered as an exact subsequence. For example:
```purescript
split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
```
Note that splitting on the empty string is a special case, which splits just before every element of the string being split. For example:
```purescript
split (onSublist "") "abc" == ["","","a","","b","","c"]
```
```purescript
split (dropDelims <<< dropBlanks $ onSublist "") "abc" == ["a","b","c"]
```

#### `whenElt`

``` purescript
whenElt :: (Char -> Boolean) -> SplitterChar
```

A splitting strategy that splits on any elements that satisfy the given predicate. For example:
```purescript
split (whenElt (\c -> c == 'a')) "asaaxsadsdaxadadaax" == ["","a","s","a","","a","xs","a","dsd","a","x","a","d","a","d","a","","a","x"]
```

#### `dropDelims`

``` purescript
dropDelims :: SplitterChar -> SplitterChar
```

Drop delimiters from the output (the default is to keep them). For example:
```purescript
split (oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
```
```purescript
split (dropDelims $ oneOf ":") "a:b:c" == ["a", "b", "c"]
``` 

#### `keepDelimsL`

``` purescript
keepDelimsL :: SplitterChar -> SplitterChar
```

Keep delimiters in the output by prepending them to adjacent chunks. For example:
```purescript
split (keepDelimsL $ oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
```

#### `keepDelimsR`

``` purescript
keepDelimsR :: SplitterChar -> SplitterChar
```

Keep delimiters in the output by appending them to adjacent chunks. For example:
```purescript
split (keepDelimsR $ oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
```

#### `condense`

``` purescript
condense :: SplitterChar -> SplitterChar
```

Condense multiple consecutive delimiters into one. For example:
```purescript
split (condense $ oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","xyz","c","x","d"]
```
```purescript
split (dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","","","c","d"]
```
```purescript
split (condense <<< dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","c","d"]
```

#### `dropInitBlank`

``` purescript
dropInitBlank :: SplitterChar -> SplitterChar
```

Don't generate a blank chunk if there is a delimiter at the beginning. For example:
```purescript
split (oneOf ":") ":a:b" == ["",":","a",":","b"]
```
```purescript
split (dropInitBlank $ oneOf ":") ":a:b" == [":","a",":","b"]
```

#### `dropFinalBlank`

``` purescript
dropFinalBlank :: SplitterChar -> SplitterChar
```

Don't generate a blank chunk if there is a delimiter at the end. For example:
```purescript
split (oneOf ":") "a:b:" == ["a",":","b",":",""]
```
```purescript
split (dropFinalBlank $ oneOf ":") "a:b:" == ["a",":","b",":"]
```

#### `dropInnerBlanks`

``` purescript
dropInnerBlanks :: SplitterChar -> SplitterChar
```

Don't generate blank chunks between consecutive delimiters. For example:
```purescript
split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
```
```purescript
split (dropInnerBlanks $ oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
`

#### `dropBlanks`

``` purescript
dropBlanks :: SplitterChar -> SplitterChar
```

Drop all blank chunks from the output, and condense consecutive delimiters into one. 
Equivalent to `dropInitBlank <<< dropFinalBlank <<< condense`. For example:
```purescript
split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
```
```purescript
split (dropBlanks $ oneOf ":") "::b:::a" == ["::","b",":::","a"]
```

#### `startsWith`

``` purescript
startsWith :: String -> SplitterChar
```

Make a strategy that splits a string into chunks that all start with the given subsequence (except possibly the first). 
Equivalent to `dropInitBlank <<< keepDelimsL <<< onSublist`. For example:
```purescript
split (startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
```

#### `startsWithOneOf`

``` purescript
startsWithOneOf :: String -> SplitterChar
```

Make a strategy that splits a string into chunks that all start with one of the given elements (except possibly the first). 
Equivalent to `dropInitBlank <<< keepDelimsL <<< oneOf`. For example:
```purescript
split (startsWithOneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
```

#### `endsWith`

``` purescript
endsWith :: String -> SplitterChar
```

Make a strategy that splits a string into chunks that all end with the given subsequence, except possibly the last. 
Equivalent to `dropFinalBlank <<< keepDelimsR <<< onSublist`. For example:
```purescript
split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
```

#### `endsWithOneOf`

``` purescript
endsWithOneOf :: String -> SplitterChar
```

Make a strategy that splits a string into chunks that all end with one of the given elements, except possibly the last. 
Equivalent to `dropFinalBlank <<< keepDelimsR <<< oneOf`. For example:
```purescript
split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
```


