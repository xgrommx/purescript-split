module Data.Split.String
    ( splitOn
    , splitOneOf
    , splitWhen
    , endBy
    , endByOneOf
    , wordsBy
    , linesBy
    , chunksOf
    , splitPlaces
    , splitPlacesBlanks
    , divvy
    , SplitterChar
    , split
    , oneOf
    , onSublist
    , whenElt
    , dropDelims
    , keepDelimsL
    , keepDelimsR
    , condense
    , dropInitBlank
    , dropFinalBlank
    , dropInnerBlanks
    , dropBlanks
    , startsWith
    , startsWithOneOf
    , endsWith
    , endsWithOneOf
    ) where

import Prelude

import Data.Split.Array (SplitterArray)
import Data.Split.Array as SA
import Data.String.CodeUnits as S

-- | A splitting strategy for an `string`.
type SplitterChar = SplitterArray Char

-- | Split a array according to the given splitting strategy. This is
-- | how to \"run\" a `SplitterChar` that has been built using the other
-- | combinators.
split :: SplitterChar -> String -> Array String
split s = map S.fromCharArray <<< SA.split s <<< S.toCharArray

-- | A splitting strategy that splits on any one of the given elements. For example:
-- | ```purescript
-- | split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
-- | ```
oneOf :: String -> SplitterChar
oneOf = SA.oneOf <<< S.toCharArray

-- | A splitting strategy that splits on the given string, when it is encountered as an exact subsequence. For example:
-- | ```purescript
-- | split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
-- | ```
-- | Note that splitting on the empty string is a special case, which splits just before every element of the string being split. For example:
-- | ```purescript
-- | split (onSublist "") "abc" == ["","","a","","b","","c"]
-- | ```
-- | ```purescript
-- | split (dropDelims <<< dropBlanks $ onSublist "") "abc" == ["a","b","c"]
-- | ```
onSublist :: String -> SplitterChar
onSublist = SA.onSublist <<< S.toCharArray

-- | A splitting strategy that splits on any elements that satisfy the given predicate. For example:
-- | ```purescript
-- | split (whenElt (\c -> c == 'a')) "asaaxsadsdaxadadaax" == ["","a","s","a","","a","xs","a","dsd","a","x","a","d","a","d","a","","a","x"]
-- | ```
whenElt :: (Char -> Boolean) -> SplitterChar
whenElt = SA.whenElt

-- | Drop delimiters from the output (the default is to keep them). For example:
-- | ```purescript
-- | split (oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
-- | ```
-- | ```purescript
-- | split (dropDelims $ oneOf ":") "a:b:c" == ["a", "b", "c"]
-- | ``` 
dropDelims :: SplitterChar -> SplitterChar
dropDelims = SA.dropDelims

-- | Keep delimiters in the output by prepending them to adjacent chunks. For example:
-- | ```purescript
-- | split (keepDelimsL $ oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
-- | ```
keepDelimsL :: SplitterChar -> SplitterChar
keepDelimsL = SA.keepDelimsL

-- | Keep delimiters in the output by appending them to adjacent chunks. For example:
-- | ```purescript
-- | split (keepDelimsR $ oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
-- | ```
keepDelimsR :: SplitterChar -> SplitterChar
keepDelimsR = SA.keepDelimsR

-- | Condense multiple consecutive delimiters into one. For example:
-- | ```purescript
-- | split (condense $ oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","xyz","c","x","d"]
-- | ```
-- | ```purescript
-- | split (dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","","","c","d"]
-- | ```
-- | ```purescript
-- | split (condense <<< dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","c","d"]
-- | ```
condense :: SplitterChar -> SplitterChar
condense = SA.condense

-- | Don't generate a blank chunk if there is a delimiter at the beginning. For example:
-- | ```purescript
-- | split (oneOf ":") ":a:b" == ["",":","a",":","b"]
-- | ```
-- | ```purescript
-- | split (dropInitBlank $ oneOf ":") ":a:b" == [":","a",":","b"]
-- | ```
dropInitBlank :: SplitterChar -> SplitterChar
dropInitBlank = SA.dropInitBlank

-- | Don't generate a blank chunk if there is a delimiter at the end. For example:
-- | ```purescript
-- | split (oneOf ":") "a:b:" == ["a",":","b",":",""]
-- | ```
-- | ```purescript
-- | split (dropFinalBlank $ oneOf ":") "a:b:" == ["a",":","b",":"]
-- | ```
dropFinalBlank :: SplitterChar -> SplitterChar
dropFinalBlank = SA.dropFinalBlank

-- | Don't generate blank chunks between consecutive delimiters. For example:
-- | ```purescript
-- | split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- | ```
-- | ```purescript
-- | split (dropInnerBlanks $ oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
-- | `
dropInnerBlanks :: SplitterChar -> SplitterChar
dropInnerBlanks = SA.dropInnerBlanks

-- | Drop all blank chunks from the output, and condense consecutive delimiters into one. 
-- | Equivalent to `dropInitBlank <<< dropFinalBlank <<< condense`. For example:
-- | ```purescript
-- | split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- | ```
-- | ```purescript
-- | split (dropBlanks $ oneOf ":") "::b:::a" == ["::","b",":::","a"]
-- | ```
dropBlanks :: SplitterChar -> SplitterChar
dropBlanks = SA.dropBlanks

-- | Make a strategy that splits a string into chunks that all start with the given subsequence (except possibly the first). 
-- | Equivalent to `dropInitBlank <<< keepDelimsL <<< onSublist`. For example:
-- | ```purescript
-- | split (startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
-- | ```
startsWith :: String -> SplitterChar
startsWith = dropInitBlank <<< keepDelimsL <<< onSublist

-- | Make a strategy that splits a string into chunks that all start with one of the given elements (except possibly the first). 
-- | Equivalent to `dropInitBlank <<< keepDelimsL <<< oneOf`. For example:
-- | ```purescript
-- | split (startsWithOneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
-- | ```
startsWithOneOf :: String -> SplitterChar
startsWithOneOf = dropInitBlank <<< keepDelimsL <<< oneOf

-- | Make a strategy that splits a string into chunks that all end with the given subsequence, except possibly the last. 
-- | Equivalent to `dropFinalBlank <<< keepDelimsR <<< onSublist`. For example:
-- | ```purescript
-- | split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
-- | ```
endsWith :: String -> SplitterChar
endsWith = dropFinalBlank <<< keepDelimsR <<< onSublist

-- | Make a strategy that splits a string into chunks that all end with one of the given elements, except possibly the last. 
-- | Equivalent to `dropFinalBlank <<< keepDelimsR <<< oneOf`. For example:
-- | ```purescript
-- | split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
-- | ```
endsWithOneOf :: String -> SplitterChar
endsWithOneOf = dropFinalBlank <<< keepDelimsR <<< oneOf

-- | Split on any of the given elements. 
-- | Equivalent to `split <<< dropDelims <<< oneOf`. For example:
-- | ```purescript
-- | splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
-- | ```
splitOneOf :: String -> String -> Array String
splitOneOf = split <<< dropDelims <<< oneOf

-- | Split on the given substrings. 
-- | Equivalent to `split <<< dropDelims <<< onSublist`. For example:
-- | ```purescript
-- | splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
-- | ```
splitOn :: String -> String -> Array String
splitOn = split <<< dropDelims <<< onSublist

-- | Split on elements satisfying the given predicate. 
-- | Equivalent to `split <<< dropDelims <<< whenElt`. For example:
-- | ```purescript
-- | splitWhen (\c -> c == 'a') "asaaxsadsdaxadadaax" == ["","s","","xs","dsd","x","d","d","","x"]
-- | ```
splitWhen :: (Char -> Boolean) -> String -> Array String
splitWhen = split <<< dropDelims <<< whenElt

-- | Split into chunks terminated by the given substrings. 
-- | Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< onSublist`. For example:
-- | ```purescript
-- | endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
-- | ```
endBy :: String -> String -> Array (String)
endBy = split <<< dropFinalBlank <<< dropDelims <<< onSublist

-- | Split into chunks terminated by one of the given elements. 
-- | Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< oneOf`. For example:
-- | ```purescript
-- | endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
-- | ```
endByOneOf :: String -> String -> Array (String)
endByOneOf = split <<< dropFinalBlank <<< dropDelims <<< oneOf

-- | Split into "words", with word boundaries indicated by the given predicate. 
-- | Equivalent to `split <<< dropBlanks <<< dropDelims <<< whenElt`. For example:
-- | ```purescript
-- | wordsBy (_=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
-- | ```
wordsBy :: (Char -> Boolean) -> String -> Array (String)
wordsBy = split <<< dropBlanks <<< dropDelims <<< whenElt

-- | Split into "lines", with line boundaries indicated by the given predicate. 
-- | Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< whenElt`. For example:
-- | ```purescript
-- | linesBy (_=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
-- | ```
linesBy :: (Char -> Boolean) -> String -> Array (String)
linesBy = split <<< dropFinalBlank <<< dropDelims <<< whenElt

-- | `chunksOf` n splits a string into length-n pieces. 
-- | The last piece will be shorter if n does not evenly divide the length of the string. 
-- | This is intentional, and is consistent with a recursive definition of `chunksOf`; it satisfies the property that
-- | ```purescript
-- | chunksOf n xs <> chunksOf n ys == chunksOf n (xs <> ys)
-- | ```
-- | whenever n evenly divides the length of xs
chunksOf :: Int -> String -> Array String
chunksOf i ls = map S.fromCharArray $ SA.chunksOf i (S.toCharArray ls)

-- | Split a string into chunks of the given lengths. For example:
-- | ```purescript
-- | splitPlaces [2,3,4] "asaaxsadsdaxadadaax" == ["as","aax","sads"]
-- | ```
-- | ```purescript
-- | splitPlaces [4,9] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa"]
-- | ```
-- | ```purescript  
-- | splitPlaces [4,9,3] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa","dad"]
-- | ```
-- | If the input string is longer than the total of the given lengths,
-- | then the remaining elements are dropped. If the string is shorter
-- | than the total of the given lengths, then the result may contain
-- | fewer chunks than requested, and the last chunk may be shorter
-- | than requested.
splitPlaces :: Array Int -> String -> Array String
splitPlaces is ys = map S.fromCharArray $ SA.splitPlaces is (S.toCharArray ys)

-- | Split a string into chunks of the given lengths. Unlike
-- | `splitPlaces`, the output string will always be the same length as
-- | the first input argument. If the input string is longer than the
-- | total of the given lengths, then the remaining elements are
-- | dropped. If the array is shorter than the total of the given
-- | lengths, then the last several chunks will be shorter than
-- | requested or empty. For example:
-- | ```purescript
-- | splitPlacesBlanks [2,3,4] "asaaxsadsdaxadadaax" == ["as","aax","sads"]
-- | ```
-- | ```purescript  
-- | splitPlacesBlanks [4,9] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa"]
-- | ```
-- | ```purescript  
-- | splitPlacesBlanks [4,9,3] "asaaxsadsdaxadadaax" == ["asaa","xsadsdaxa","dad"]
-- | ```
-- | Notice the empty array in the output of the third example, which
-- | differs from the behavior of `splitPlaces`.
splitPlacesBlanks :: Array Int -> String -> Array String
splitPlacesBlanks is ys = map S.fromCharArray $ SA.splitPlacesBlanks is (S.toCharArray ys)

-- | Divides up an input string into a set of substrings, according to `n` and `m`
-- | input specifications you provide. Each substring will have `n` items, and the
-- | start of each substring will be offset by `m` items from the previous one.
-- | ```purescript
-- | divvy 5 5 "asaaxsadsdaxadadaax" == ["asaax","sadsd","axada"]
-- | ```
-- | In the case where a source strings's trailing elements do no fill an entire
-- | substring, those trailing elements will be dropped.
-- | ```purescript
-- | divvy 5 2 "asaaxsadsdaxadadaax" == ["asaax","aaxsa","xsads","adsda","sdaxa","axada","adada","adaax"]
-- | ```
divvy :: Int -> Int -> String -> Array String
divvy n m s = map S.fromCharArray $ SA.divvy n m (S.toCharArray s)