## Module Data.Split.Array

#### `splitOn`

``` purescript
splitOn :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

Split on the given sublist. 
Equivalent to `split <<< dropDelims <<< onSublist`. For example:
```purescript
splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
```

#### `splitOneOf`

``` purescript
splitOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

Split on any of the given elements. 
Equivalent to `split <<< dropDelims <<< oneOf`. For example:
```purescript
splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
```

#### `splitWhen`

``` purescript
splitWhen :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
```

Split on elements satisfying the given predicate. 
Equivalent to `split <<< dropDelims <<< whenElt`. For example:
```purescript
splitWhen (_<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
```

#### `endBy`

``` purescript
endBy :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

Split into chunks terminated by the given subsequence. 
Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< onSublist`. For example:
```purescript
endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
```

#### `endByOneOf`

``` purescript
endByOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
```

Split into chunks terminated by one of the given elements. 
Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< oneOf`. For example:
```purescript
endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
```

#### `wordsBy`

``` purescript
wordsBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
```

Split into "words", with word boundaries indicated by the given predicate. 
Equivalent to `split <<< dropBlanks <<< dropDelims <<< whenElt`. For example:
```purescript
wordsBy (_=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
```

#### `linesBy`

``` purescript
linesBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
```

Split into "lines", with line boundaries indicated by the given predicate. 
Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< whenElt`. For example:
```purescript
linesBy (_=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
```

#### `chunksOf`

``` purescript
chunksOf :: forall e. Int -> Array e -> Array (Array e)
```

`chunksOf` n splits an array into length-n pieces. 
The last piece will be shorter if n does not evenly divide the length of the array. 
If n <= 0, `chunksOf` n l returns an infinite array of empty arrays. For example:
Note that `chunksOf` n [] is [], not [[]]. 
This is intentional, and is consistent with a recursive definition of `chunksOf`; it satisfies the property that
```purescript
chunksOf n xs <> chunksOf n ys == chunksOf n (xs <> ys)
```
whenever n evenly divides the length of xs

#### `splitPlaces`

``` purescript
splitPlaces :: forall e. Array Int -> Array e -> Array (Array e)
```

Split a array into chunks of the given lengths. For example:
```purescript
splitPlaces [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
```
```purescript
splitPlaces [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
```
```purescript  
splitPlaces [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
```
If the input array is longer than the total of the given lengths,
then the remaining elements are dropped. If the array is shorter
than the total of the given lengths, then the result may contain
fewer chunks than requested, and the last chunk may be shorter
than requested.

#### `splitPlacesBlanks`

``` purescript
splitPlacesBlanks :: forall e. Array Int -> Array e -> Array (Array e)
```

Split a array into chunks of the given lengths. Unlike
`splitPlaces`, the output array will always be the same length as
the first input argument. If the input array is longer than the
total of the given lengths, then the remaining elements are
dropped. If the array is shorter than the total of the given
lengths, then the last several chunks will be shorter than
requested or empty. For example:
```purescript
splitPlacesBlanks [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
```
```purescript  
splitPlacesBlanks [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
```
```purescript  
splitPlacesBlanks [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10],[]]
```
Notice the empty array in the output of the third example, which
differs from the behavior of `splitPlaces`.

#### `chop`

``` purescript
chop :: forall a b. (Array a -> { left :: b, right :: Array a }) -> Array a -> Array b
```

#### `divvy`

``` purescript
divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
```

Divides up an input array into a set of subarrays, according to `n` and `m`
input specifications you provide. Each subarray will have `n` items, and the
start of each subarray will be offset by `m` items from the previous one.
```purescript
divvy 5 5 (1..20) == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
```
In the case where a source arrays's trailing elements do no fill an entire
subarray, those trailing elements will be dropped.
```purescript
divvy 5 2 (1..10) == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
```
As an example, you can generate a moving average over a array of prices:
```purescript
type Prices = Array Number
type AveragePrices = Array Number

average :: Array Number -> Number
average xs = sum xs / length xs

simpleMovingAverage :: Prices -> AveragePrices
simpleMovingAverage priceArray =
  map average divvyedPrices
    where divvyedPrices = divvy 20 1 priceArray
```

#### `SplitterArray`

``` purescript
type SplitterArray a = Splitter Array a
```

A splitting strategy for an `array`.

#### `defaultSplitter`

``` purescript
defaultSplitter :: forall a. SplitterArray a
```

The default splitting strategy: 
keep delimiters in the output as separate chunks, don't condense multiple consecutive delimiters into one, keep initial and final blank chunks. 
Default delimiter is the constantly false predicate.
Note that `defaultSplitter` should normally not be used; 
use `oneOf`, `onSublist`, or `whenElt` instead, which are the same as the `defaultSplitter` with just the delimiter overridden.
The `defaultSplitter` strategy with any delimiter gives a maximally information-preserving splitting strategy, 
in the sense that (a) taking the concat of the output yields the original array, 
and (b) given only the output array, we can reconstruct a Splitter which would produce the same output array again given the original input list. 
This default strategy can be overridden to allow discarding various sorts of information.

#### `split`

``` purescript
split :: forall a. SplitterArray a -> Array a -> Array (Array a)
```

Split a array according to the given splitting strategy. This is
how to \"run\" a `SplitterArray` that has been built using the other
combinators.

#### `oneOf`

``` purescript
oneOf :: forall a. Eq a => Array a -> SplitterArray a
```

A splitting strategy that splits on any one of the given elements. For example:
```purescript
split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
```  

#### `onSublist`

``` purescript
onSublist :: forall a. Eq a => Array a -> SplitterArray a
```

A splitting strategy that splits on the given array, when it is encountered as an exact subsequence. For example:
```purescript
split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
```
Note that splitting on the empty array is a special case, which splits just before every element of the array being split. For example:
```purescript
split (onSublist "") "abc" == ["","","a","","b","","c"]
```
```purescript
split (dropDelims <<< dropBlanks $ onSublist "") "abc" == ["a","b","c"]
```
However, if you want to break a array into singleton elements like this, you are better off using `chunksOf 1`, or better yet, `map (:[])`

#### `whenElt`

``` purescript
whenElt :: forall a. (a -> Boolean) -> SplitterArray a
```

A splitting strategy that splits on any elements that satisfy the given predicate. For example:
```purescript
split (whenElt (_<0)) [2,4,-3,6,-9,1] == [[2,4],[-3],[6],[-9],[1]]
```

#### `dropDelims`

``` purescript
dropDelims :: forall a. SplitterArray a -> SplitterArray a
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
keepDelimsL :: forall a. SplitterArray a -> SplitterArray a
```

Keep delimiters in the output by prepending them to adjacent chunks. For example:
```purescript
split (keepDelimsL $ oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
```

#### `keepDelimsR`

``` purescript
keepDelimsR :: forall a. SplitterArray a -> SplitterArray a
```

Keep delimiters in the output by appending them to adjacent chunks. For example:
```purescript
split (keepDelimsR $ oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
```

#### `condense`

``` purescript
condense :: forall a. SplitterArray a -> SplitterArray a
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
dropInitBlank :: forall a. SplitterArray a -> SplitterArray a
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
dropFinalBlank :: forall a. SplitterArray a -> SplitterArray a
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
dropInnerBlanks :: forall a. SplitterArray a -> SplitterArray a
```

Don't generate blank chunks between consecutive delimiters. For example:
```purescript
split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
```
```purescript
split (dropInnerBlanks $ oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
```

#### `dropBlanks`

``` purescript
dropBlanks :: forall a. SplitterArray a -> SplitterArray a
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
startsWith :: forall a. Eq a => Array a -> SplitterArray a
```

Make a strategy that splits a array into chunks that all start with the given subsequence (except possibly the first). 
Equivalent to `dropInitBlank <<< keepDelimsL <<< onSublist`. For example:
```purescript
split (startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
```

#### `startsWithOneOf`

``` purescript
startsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
```

Make a strategy that splits a array into chunks that all start with one of the given elements (except possibly the first). 
Equivalent to `dropInitBlank <<< keepDelimsL <<< oneOf`. For example:
```purescript
split (startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
```

#### `endsWith`

``` purescript
endsWith :: forall a. Eq a => Array a -> SplitterArray a
```

Make a strategy that splits an array into chunks that all end with the given subsequence, except possibly the last. 
Equivalent to `dropFinalBlank <<< keepDelimsR <<< onSublist`. For example:
```purescript
split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
```

#### `endsWithOneOf`

``` purescript
endsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
```

Make a strategy that splits an array into chunks that all end with one of the given elements, except possibly the last. 
Equivalent to `dropFinalBlank <<< keepDelimsR <<< oneOf`. For example:
```purescript
split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
```


