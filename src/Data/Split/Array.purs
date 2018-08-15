module Data.Split.Array 
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
    , chop
    , divvy
    , SplitterArray
    , defaultSplitter
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

import Data.Array (concatMap, drop, filter, length, span, take, uncons, (:))
import Data.Common (Chunk(..), CondensePolicy(..), DelimPolicy(..), Delimiter(..), EndPolicy(..), Splitter, fromElem, isDelim, isText)
import Data.Foldable (null, elem)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

-- | A delimiter is an `array` of predicates on elements, matched by some contiguous subsequence of an `array`.
type DelimiterArray = Delimiter Array
-- | Tag chunks as delimiters or text.
type ChunkArray = Chunk Array
-- | 
type SplitArray a = Array (ChunkArray a)
-- | A splitting strategy for an `array`.
type SplitterArray a = Splitter Array a

-- | The default splitting strategy: 
-- | keep delimiters in the output as separate chunks, don't condense multiple consecutive delimiters into one, keep initial and final blank chunks. 
-- | Default delimiter is the constantly false predicate.
-- | Note that `defaultSplitter` should normally not be used; 
-- | use `oneOf`, `onSublist`, or `whenElt` instead, which are the same as the `defaultSplitter` with just the delimiter overridden.
-- | The `defaultSplitter` strategy with any delimiter gives a maximally information-preserving splitting strategy, 
-- | in the sense that (a) taking the concat of the output yields the original array, 
-- | and (b) given only the output array, we can reconstruct a Splitter which would produce the same output array again given the original input list. 
-- | This default strategy can be overridden to allow discarding various sorts of information.
defaultSplitter :: forall a. SplitterArray a
defaultSplitter 
  = { delimiter: Delimiter [const false]
    , delimPolicy: Keep
    , condensePolicy: KeepBlankFields
    , initBlankPolicy: KeepBlank
    , finalBlankPolicy: KeepBlank }

-- | Try to match a delimiter at the start of an array, either failing or decomposing the array into the portion which matched the delimiter and the remainder.
matchDelim :: forall a. DelimiterArray a -> Array a -> Maybe { left :: Array a, right :: Array a }
matchDelim (Delimiter fs) xs = case uncons fs, uncons xs of
  Nothing, _ -> Just { left: [], right: xs }
  _, Nothing -> Nothing
  Just r1, Just r2
    | r1.head r2.head -> matchDelim (Delimiter r1.tail) r2.tail >>= \t -> Just ({left: r2.head : t.left, right: t.right})
    | otherwise -> Nothing

breakDelim :: forall a. DelimiterArray a -> Array a -> Tuple (Array a) (Maybe { left :: Array a, right :: Array a })
breakDelim d@(Delimiter fs) xs = case uncons fs, uncons xs of
  Nothing, _ -> Tuple [] (Just { left: [], right: xs })
  _, Nothing -> Tuple [] Nothing
  Just _, Just r -> case matchDelim d xs of
    Nothing -> let (Tuple ys match) = breakDelim d r.tail in Tuple (r.head : ys) match
    j@(Just _) -> Tuple [] j

-- | Given a delimiter to use, split an array into an internal representation with chunks tagged as delimiters or text. 
-- | This transformation is lossless; in particular
-- | ```purescript
-- | concatMap fromElem (splitInternal d l) == l
-- | ```
splitInternal :: forall a. DelimiterArray a -> Array a -> SplitArray a
splitInternal d xss = case uncons xss of
  Nothing -> []
  Just r
    | null xs -> toSplitList match
    | otherwise -> Text xs : toSplitList match
    where
      Tuple xs match = breakDelim d xss

      toSplitList Nothing = []
      toSplitList (Just {left, right}) = case uncons left, uncons right of
        Nothing, Just {head: r, tail: rs} -> Delim [] : Text [r] : splitInternal d rs
        _, _ -> Delim left : splitInternal d right

-- | Drop delimiters if the `DelimPolicy` is `Drop`.
doDrop :: forall a. DelimPolicy -> SplitArray a -> SplitArray a
doDrop Drop l = filter isText l
doDrop _ l = l

-- Condense multiple consecutive delimiters into one if the `CondensePolicy` is `Condense`.
doCondense :: forall a. CondensePolicy -> SplitArray a -> SplitArray a
doCondense Condense ls = condense' ls
  where
    condense' ls' = case uncons ls' of
      Nothing -> []
      Just {head, tail}
        | isText head -> head : condense' tail
        | otherwise -> (Delim $ concatMap fromElem r.init) : condense' r.rest
          where
            r = span isDelim ls'
doCondense _ ls = ls

-- | Drop a final blank chunk according to the given `EndPolicy`.
dropFinal :: forall a. EndPolicy -> SplitArray a -> SplitArray a
dropFinal e l = case e, uncons l of
  DropBlank, _ -> dropFinal' l
  _, Nothing -> []
  _, (Just _) -> l
    where
      dropFinal' l' = case uncons l' of
        Nothing -> []
        Just r@{head: Text t, tail} 
          | null t && null r.tail -> []
        Just r -> r.head : dropFinal' r.tail

-- | Drop an initial blank chunk according to the given `EndPolicy`.
dropInitial :: forall a. EndPolicy -> SplitArray a -> SplitArray a
dropInitial e l = case e, uncons l of
  DropBlank, Just { head: Text t, tail }
    | null t -> tail
  _, _ -> l

-- | Merge delimiters with adjacent chunks to the left.
mergeRight :: SplitArray ~> SplitArray
mergeRight xs = case uncons xs of
  Nothing -> []
  Just {head: Text c, tail} -> let (Tuple d lTail) = go tail in Text (c <> d) : mergeRight lTail
  Just r -> r.head : mergeRight r.tail 
    where
      go tail = case uncons tail of
        Just { head: Delim d', tail: tail' } -> Tuple d' tail'
        _ -> Tuple [] tail

-- | Merge delimiters with adjacent chunks to the right (yes, that's not a typo: the delimiters should end up on the left of the chunks, so they are merged with chunks to their right).
mergeLeft :: SplitArray ~> SplitArray
mergeLeft xs = case uncons xs of
  Nothing -> []
  Just { head: h1@(Delim d), tail: t1 } -> case uncons t1 of
    Just { head: Text c, tail: t2 } -> Text (d <> c) : mergeLeft t2
    _ -> h1 : mergeLeft t1
  Just r -> r.head : mergeLeft r.tail

-- | Merge delimiters into adjacent chunks according to the `DelimPolicy`.
doMerge :: forall a. DelimPolicy -> SplitArray a -> SplitArray a
doMerge KeepLeft = mergeLeft
doMerge KeepRight = mergeRight
doMerge _ = identity

-- | Insert blank chunks between consecutive delimiters.
insertBlanks' :: forall a. CondensePolicy -> SplitArray a -> SplitArray a
insertBlanks' cp l = case cp, uncons l of
  _, Nothing -> []
  DropBlankFields, Just r@{head: d1@(Delim _), tail: t1} -> case uncons t1 of
    Just { head: d2@(Delim _), tail: t2} -> d1 : insertBlanks' cp (d2 : t2)
    _ -> r.head : insertBlanks' cp r.tail
  _, Just r@{head: d1@(Delim _), tail: t1} -> case uncons t1 of
    Nothing -> [d1, Text []]
    Just { head: d2@(Delim _), tail: t2} -> d1 : Text [] : insertBlanks' cp (d2 : t2)
    _ -> r.head : insertBlanks' cp r.tail
  _, Just r -> r.head : insertBlanks' cp r.tail

-- | Insert blank chunks between any remaining consecutive delimiters (unless the condense policy is `DropBlankFields`), 
-- | and at the beginning or end if the first or last element is a delimiter.
insertBlanks :: forall a. CondensePolicy -> SplitArray a -> SplitArray a
insertBlanks cp xs = case cp, uncons xs of
  _, Nothing -> [Text []]
  _, Just { head: d@(Delim _), tail } -> Text [] : insertBlanks' cp (d : tail)
  _, _ -> insertBlanks' cp xs

-- | Given a split array in the internal tagged representation, produce
-- | a new internal tagged representation corresponding to the final
-- | output, according to the strategy defined by the given
-- | `SplitterArray`.
postProcess :: forall a. SplitterArray a -> SplitArray a -> SplitArray a
postProcess s 
  = dropFinal (s.finalBlankPolicy) 
  <<< dropInitial (s.initBlankPolicy) 
  <<< doMerge (s.delimPolicy) 
  <<< doDrop (s.delimPolicy) 
  <<< insertBlanks (s.condensePolicy) 
  <<< doCondense (s.condensePolicy)

-- | Split a array according to the given splitting strategy. This is
-- | how to \"run\" a `SplitterArray` that has been built using the other
-- | combinators.
split :: forall a. SplitterArray a -> Array a -> Array (Array a)
split s = map fromElem <<< postProcess s <<< splitInternal (s.delimiter)

-- | A splitting strategy that splits on any one of the given elements. For example:
-- | ```purescript
-- | split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
-- | ```  
oneOf :: forall a. Eq a => Array a -> SplitterArray a
oneOf elts = defaultSplitter { delimiter = Delimiter [(\x -> x `elem` elts)] }

-- | A splitting strategy that splits on the given array, when it is encountered as an exact subsequence. For example:
-- | ```purescript
-- | split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
-- | ```
-- | Note that splitting on the empty array is a special case, which splits just before every element of the array being split. For example:
-- | ```purescript
-- | split (onSublist "") "abc" == ["","","a","","b","","c"]
-- | ```
-- | ```purescript
-- | split (dropDelims <<< dropBlanks $ onSublist "") "abc" == ["a","b","c"]
-- | ```
-- | However, if you want to break a array into singleton elements like this, you are better off using `chunksOf 1`, or better yet, `map (:[])`
onSublist :: forall a. Eq a => Array a -> SplitterArray a
onSublist lst = defaultSplitter { delimiter = Delimiter (map (==) lst) }

-- | A splitting strategy that splits on any elements that satisfy the given predicate. For example:
-- | ```purescript
-- | split (whenElt (_<0)) [2,4,-3,6,-9,1] == [[2,4],[-3],[6],[-9],[1]]
-- | ```
whenElt :: forall a. (a -> Boolean) -> SplitterArray a
whenElt p = defaultSplitter { delimiter = Delimiter [p] }

-- | Drop delimiters from the output (the default is to keep them). For example:
-- | ```purescript
-- | split (oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
-- | ```
-- | ```purescript
-- | split (dropDelims $ oneOf ":") "a:b:c" == ["a", "b", "c"]
-- | ```  
dropDelims :: forall a. SplitterArray a -> SplitterArray a
dropDelims s = s { delimPolicy = Drop }

-- | Keep delimiters in the output by prepending them to adjacent chunks. For example:
-- | ```purescript
-- | split (keepDelimsL $ oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
-- | ```
keepDelimsL :: forall a. SplitterArray a -> SplitterArray a
keepDelimsL s = s { delimPolicy = KeepLeft }

-- | Keep delimiters in the output by appending them to adjacent chunks. For example:
-- | ```purescript
-- | split (keepDelimsR $ oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
-- | ```
keepDelimsR :: forall a. SplitterArray a -> SplitterArray a
keepDelimsR s = s { delimPolicy = KeepRight }

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
condense :: forall a. SplitterArray a -> SplitterArray a
condense s = s { condensePolicy = Condense }

-- | Don't generate a blank chunk if there is a delimiter at the beginning. For example:
-- | ```purescript
-- | split (oneOf ":") ":a:b" == ["",":","a",":","b"]
-- | ```
-- | ```purescript
-- | split (dropInitBlank $ oneOf ":") ":a:b" == [":","a",":","b"]
-- | ```
dropInitBlank :: forall a. SplitterArray a -> SplitterArray a
dropInitBlank s = s { initBlankPolicy = DropBlank }

-- | Don't generate a blank chunk if there is a delimiter at the end. For example:
-- | ```purescript
-- | split (oneOf ":") "a:b:" == ["a",":","b",":",""]
-- | ```
-- | ```purescript
-- | split (dropFinalBlank $ oneOf ":") "a:b:" == ["a",":","b",":"]
-- | ```
dropFinalBlank :: forall a. SplitterArray a -> SplitterArray a
dropFinalBlank s = s { finalBlankPolicy = DropBlank }

-- | Don't generate blank chunks between consecutive delimiters. For example:
-- | ```purescript
-- | split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- | ```
-- | ```purescript
-- | split (dropInnerBlanks $ oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
-- | ```
dropInnerBlanks :: forall a. SplitterArray a -> SplitterArray a
dropInnerBlanks s = s { condensePolicy = DropBlankFields }

-- | Drop all blank chunks from the output, and condense consecutive delimiters into one. 
-- | Equivalent to `dropInitBlank <<< dropFinalBlank <<< condense`. For example:
-- | ```purescript
-- | split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- | ```
-- | ```purescript
-- | split (dropBlanks $ oneOf ":") "::b:::a" == ["::","b",":::","a"]
-- | ```
dropBlanks :: forall a. SplitterArray a -> SplitterArray a
dropBlanks = dropInitBlank <<< dropFinalBlank <<< condense

-- | Make a strategy that splits a array into chunks that all start with the given subsequence (except possibly the first). 
-- | Equivalent to `dropInitBlank <<< keepDelimsL <<< onSublist`. For example:
-- | ```purescript
-- | split (startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
-- | ```
startsWith :: forall a. Eq a => Array a -> SplitterArray a
startsWith = dropInitBlank <<< keepDelimsL <<< onSublist

-- | Make a strategy that splits a array into chunks that all start with one of the given elements (except possibly the first). 
-- | Equivalent to `dropInitBlank <<< keepDelimsL <<< oneOf`. For example:
-- | ```purescript
-- | split (startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
-- | ```
startsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
startsWithOneOf = dropInitBlank <<< keepDelimsL <<< oneOf

-- | Make a strategy that splits an array into chunks that all end with the given subsequence, except possibly the last. 
-- | Equivalent to `dropFinalBlank <<< keepDelimsR <<< onSublist`. For example:
-- | ```purescript
-- | split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
-- | ```
endsWith :: forall a. Eq a => Array a -> SplitterArray a
endsWith = dropFinalBlank <<< keepDelimsR <<< onSublist

-- | Make a strategy that splits an array into chunks that all end with one of the given elements, except possibly the last. 
-- | Equivalent to `dropFinalBlank <<< keepDelimsR <<< oneOf`. For example:
-- | ```purescript
-- | split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
-- | ```
endsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
endsWithOneOf = dropFinalBlank <<< keepDelimsR <<< oneOf

-- | Split on any of the given elements. 
-- | Equivalent to `split <<< dropDelims <<< oneOf`. For example:
-- | ```purescript
-- | splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
-- | ```
splitOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
splitOneOf = split <<< dropDelims <<< oneOf

-- | Split on the given sublist. 
-- | Equivalent to `split <<< dropDelims <<< onSublist`. For example:
-- | ```purescript
-- | splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
-- | ```
splitOn :: forall a. Eq a => Array a -> Array a -> Array (Array a)
splitOn = split <<< dropDelims <<< onSublist

-- | Split on elements satisfying the given predicate. 
-- | Equivalent to `split <<< dropDelims <<< whenElt`. For example:
-- | ```purescript
-- | splitWhen (_<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
-- | ```
splitWhen :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
splitWhen = split <<< dropDelims <<< whenElt

-- | Split into chunks terminated by the given subsequence. 
-- | Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< onSublist`. For example:
-- | ```purescript
-- | endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
-- | ```
endBy :: forall a. Eq a => Array a -> Array a -> Array (Array a)
endBy = split <<< dropFinalBlank <<< dropDelims <<< onSublist

-- | Split into chunks terminated by one of the given elements. 
-- | Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< oneOf`. For example:
-- | ```purescript
-- | endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
-- | ```
endByOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
endByOneOf = split <<< dropFinalBlank <<< dropDelims <<< oneOf

-- | Split into "words", with word boundaries indicated by the given predicate. 
-- | Equivalent to `split <<< dropBlanks <<< dropDelims <<< whenElt`. For example:
-- | ```purescript
-- | wordsBy (_=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
-- | ```
wordsBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
wordsBy = split <<< dropBlanks <<< dropDelims <<< whenElt

-- | Split into "lines", with line boundaries indicated by the given predicate. 
-- | Equivalent to `split <<< dropFinalBlank <<< dropDelims <<< whenElt`. For example:
-- | ```purescript
-- | linesBy (_=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
-- | ```
linesBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
linesBy = split <<< dropFinalBlank <<< dropDelims <<< whenElt

build :: forall a. ((a -> Array a -> Array a) -> Array a -> Array a) -> Array a
build g = g (\x xs -> x : xs) []

-- | `chunksOf` n splits an array into length-n pieces. 
-- | The last piece will be shorter if n does not evenly divide the length of the array. 
-- | If n <= 0, `chunksOf` n l returns an infinite array of empty arrays. For example:
-- | Note that `chunksOf` n [] is [], not [[]]. 
-- | This is intentional, and is consistent with a recursive definition of `chunksOf`; it satisfies the property that
-- | ```purescript
-- | chunksOf n xs <> chunksOf n ys == chunksOf n (xs <> ys)
-- | ```
-- | whenever n evenly divides the length of xs

chunksOf :: forall e. Int -> Array e -> Array (Array e)
chunksOf i ls
  | i < 0 = unsafeCrashWith "index should be bigger than 0"
  | otherwise = map (take i) (build (splitter ls))
    where
      splitter :: forall a. Array e -> (Array e -> a -> a) -> a -> a
      splitter l c n = case uncons l of
        Nothing -> n
        Just _ -> l `c` splitter (drop i l) c n

splitAt :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt n xs = { before: take n xs, after: drop n xs }

-- | Split a array into chunks of the given lengths. For example:
-- | ```purescript
-- | splitPlaces [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
-- | ```
-- | ```purescript
-- | splitPlaces [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
-- | ```
-- | ```purescript  
-- | splitPlaces [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
-- | ```
-- | If the input array is longer than the total of the given lengths,
-- | then the remaining elements are dropped. If the array is shorter
-- | than the total of the given lengths, then the result may contain
-- | fewer chunks than requested, and the last chunk may be shorter
-- | than requested.
splitPlaces :: forall e. Array Int -> Array e -> Array (Array e)
splitPlaces is ys = build (splitPlacer is ys)
  where
    splitPlacer :: forall b t. Array Int -> Array b -> (Array b -> t -> t) -> t -> t
    splitPlacer xss yss c n = case uncons xss, uncons yss of
      Nothing, _ -> n
      _, Nothing -> n
      Just {head, tail}, _ -> let {before, after} = splitAt head yss in before `c` splitPlacer tail after c n

-- | Split a array into chunks of the given lengths. Unlike
-- | `splitPlaces`, the output array will always be the same length as
-- | the first input argument. If the input array is longer than the
-- | total of the given lengths, then the remaining elements are
-- | dropped. If the array is shorter than the total of the given
-- | lengths, then the last several chunks will be shorter than
-- | requested or empty. For example:
-- | ```purescript
-- | splitPlacesBlanks [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
-- | ```
-- | ```purescript  
-- | splitPlacesBlanks [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
-- | ```
-- | ```purescript  
-- | splitPlacesBlanks [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10],[]]
-- | ```
-- | Notice the empty array in the output of the third example, which
-- | differs from the behavior of `splitPlaces`.
splitPlacesBlanks :: forall e. Array Int -> Array e -> Array (Array e)
splitPlacesBlanks is ys = build (splitPlacer is ys)
  where
    splitPlacer :: forall b t. Array Int -> Array b -> (Array b -> t -> t) -> t -> t
    splitPlacer xss yss c n = case uncons xss, uncons yss of
      Nothing, _ -> n
      Just {head, tail}, _ -> let {before, after} = splitAt head yss in before `c` splitPlacer tail after c n

chop :: forall a b. (Array a -> { left:: b, right :: Array a }) -> Array a -> Array b
chop f as = case uncons as of
  Nothing -> []
  Just _ -> let r = f as in r.left : chop f r.right

-- | Divides up an input array into a set of subarrays, according to `n` and `m`
-- | input specifications you provide. Each subarray will have `n` items, and the
-- | start of each subarray will be offset by `m` items from the previous one.
-- | ```purescript
-- | divvy 5 5 (1..20) == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
-- | ```
-- | In the case where a source arrays's trailing elements do no fill an entire
-- | subarray, those trailing elements will be dropped.
-- | ```purescript
-- | divvy 5 2 (1..10) == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
-- | ```
-- | As an example, you can generate a moving average over a array of prices:
-- | ```purescript
-- | type Prices = Array Number
-- | type AveragePrices = Array Number
-- | 
-- | average :: Array Number -> Number
-- | average xs = sum xs / length xs
-- | 
-- | simpleMovingAverage :: Prices -> AveragePrices
-- | simpleMovingAverage priceArray =
-- |   map average divvyedPrices
-- |     where divvyedPrices = divvy 20 1 priceArray
-- | ```

divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
divvy n m l = case uncons l of
  Nothing -> []
  Just _ -> filter (\ws -> (n == length ws)) choppedl
    where choppedl = chop (\xs -> { left: take n xs , right: drop m xs }) l
