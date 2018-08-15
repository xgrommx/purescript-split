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
import Data.Common (Chunk(..), CondensePolicy(..), DelimPolicy(..), Delimiter(..), EndPolicy(..), Splitter)
import Data.Foldable (null, elem)
import Data.Maybe (Maybe(..))
import Data.Split.List (divvy)
import Data.Tuple (Tuple(..))

type DelimiterArray = Delimiter Array
type ChunkArray = Chunk Array
type SplitArray a = Array (ChunkArray a)
type SplitterArray a = Splitter Array a

defaultSplitter :: forall a. SplitterArray a
defaultSplitter 
  = { delimiter: Delimiter [const false]
    , delimPolicy: Keep
    , condensePolicy: KeepBlankFields
    , initBlankPolicy: KeepBlank
    , finalBlankPolicy: KeepBlank }

matchDelim :: forall a. DelimiterArray a -> Array a -> Maybe { left :: Array a, right :: Array a }
matchDelim (Delimiter fs) xs = case uncons fs, uncons xs of
  Nothing, _ -> Just { left: [], right: xs }
  _, Nothing -> Nothing
  Just r1, Just r2
    | r1.head r2.head -> matchDelim (Delimiter r1.tail) r2.tail >>= \t -> Just ({left: r2.head : t.left, right: t.right})
    | otherwise -> Nothing

fromElem :: ChunkArray ~> Array
fromElem (Text as) = as
fromElem (Delim as) = as

isDelim :: forall a. ChunkArray a -> Boolean
isDelim (Delim _) = true
isDelim _ = false

isText :: forall a. ChunkArray a -> Boolean
isText (Text _) = true
isText _ = false

breakDelim :: forall a. DelimiterArray a -> Array a -> Tuple (Array a) (Maybe { left :: Array a, right :: Array a })
breakDelim d@(Delimiter fs) xs = case uncons fs, uncons xs of
  Nothing, _ -> Tuple [] (Just { left: [], right: xs })
  _, Nothing -> Tuple [] Nothing
  Just _, Just r -> case matchDelim d xs of
    Nothing -> let (Tuple ys match) = breakDelim d r.tail in Tuple (r.head : ys) match
    j@(Just _) -> Tuple [] j

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

doDrop :: forall a. DelimPolicy -> SplitArray a -> SplitArray a
doDrop Drop l = filter isText l
doDrop _ l = l

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

dropInitial :: forall a. EndPolicy -> SplitArray a -> SplitArray a
dropInitial e l = case e, uncons l of
  DropBlank, Just { head: Text t, tail }
    | null t -> tail
  _, _ -> l

mergeRight :: SplitArray ~> SplitArray
mergeRight xs = case uncons xs of
  Nothing -> []
  Just {head: Text c, tail} -> let (Tuple d lTail) = go tail in Text (c <> d) : mergeRight lTail
  Just r -> r.head : mergeRight r.tail 
    where
      go tail = case uncons tail of
        Just { head: Delim d', tail: tail' } -> Tuple d' tail'
        _ -> Tuple [] tail

mergeLeft :: SplitArray ~> SplitArray
mergeLeft xs = case uncons xs of
  Nothing -> []
  Just { head: h1@(Delim d), tail: t1 } -> case uncons t1 of
    Just { head: Text c, tail: t2 } -> Text (d <> c) : mergeLeft t2
    _ -> h1 : mergeLeft t1
  Just r -> r.head : mergeLeft r.tail

doMerge :: forall a. DelimPolicy -> SplitArray a -> SplitArray a
doMerge KeepLeft = mergeLeft
doMerge KeepRight = mergeRight
doMerge _ = identity

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

insertBlanks :: forall a. CondensePolicy -> SplitArray a -> SplitArray a
insertBlanks cp xs = case cp, uncons xs of
  _, Nothing -> [Text []]
  _, Just { head: d@(Delim _), tail } -> Text [] : insertBlanks' cp (d : tail)
  _, _ -> insertBlanks' cp xs

postProcess :: forall a. SplitterArray a -> SplitArray a -> SplitArray a
postProcess s 
  = dropFinal (s.finalBlankPolicy) 
  <<< dropInitial (s.initBlankPolicy) 
  <<< doMerge (s.delimPolicy) 
  <<< doDrop (s.delimPolicy) 
  <<< insertBlanks (s.condensePolicy) 
  <<< doCondense (s.condensePolicy)

split :: forall a. SplitterArray a -> Array a -> Array (Array a)
split s = map fromElem <<< postProcess s <<< splitInternal (s.delimiter)

oneOf :: forall a. Eq a => Array a -> SplitterArray a
oneOf elts = defaultSplitter { delimiter = Delimiter [(\x -> x `elem` elts)] }

onSublist :: forall a. Eq a => Array a -> SplitterArray a
onSublist lst = defaultSplitter { delimiter = Delimiter (map (==) lst) }

whenElt :: forall a. (a -> Boolean) -> SplitterArray a
whenElt p = defaultSplitter { delimiter = Delimiter [p] }

dropDelims :: forall a. SplitterArray a -> SplitterArray a
dropDelims s = s { delimPolicy = Drop }

keepDelimsL :: forall a. SplitterArray a -> SplitterArray a
keepDelimsL s = s { delimPolicy = KeepLeft }

keepDelimsR :: forall a. SplitterArray a -> SplitterArray a
keepDelimsR s = s { delimPolicy = KeepRight }

condense :: forall a. SplitterArray a -> SplitterArray a
condense s = s { condensePolicy = Condense }

dropInitBlank :: forall a. SplitterArray a -> SplitterArray a
dropInitBlank s = s { initBlankPolicy = DropBlank }

dropFinalBlank :: forall a. SplitterArray a -> SplitterArray a
dropFinalBlank s = s { finalBlankPolicy = DropBlank }

dropInnerBlanks :: forall a. SplitterArray a -> SplitterArray a
dropInnerBlanks s = s { condensePolicy = DropBlankFields }

dropBlanks :: forall a. SplitterArray a -> SplitterArray a
dropBlanks = dropInitBlank <<< dropFinalBlank <<< condense

startsWith :: forall a. Eq a => Array a -> SplitterArray a
startsWith = dropInitBlank <<< keepDelimsL <<< onSublist

startsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
startsWithOneOf = dropInitBlank <<< keepDelimsL <<< oneOf

endsWith :: forall a. Eq a => Array a -> SplitterArray a
endsWith = dropFinalBlank <<< keepDelimsR <<< onSublist

endsWithOneOf :: forall a. Eq a => Array a -> SplitterArray a
endsWithOneOf = dropFinalBlank <<< keepDelimsR <<< oneOf

splitOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
splitOneOf = split <<< dropDelims <<< oneOf

splitOn :: forall a. Eq a => Array a -> Array a -> Array (Array a)
splitOn = split <<< dropDelims <<< onSublist

splitWhen :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
splitWhen = split <<< dropDelims <<< whenElt

endBy :: forall a. Eq a => Array a -> Array a -> Array (Array a)
endBy = split <<< dropFinalBlank <<< dropDelims <<< onSublist

endByOneOf :: forall a. Eq a => Array a -> Array a -> Array (Array a)
endByOneOf = split <<< dropFinalBlank <<< dropDelims <<< oneOf

wordsBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
wordsBy = split <<< dropBlanks <<< dropDelims <<< whenElt

linesBy :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
linesBy = split <<< dropFinalBlank <<< dropDelims <<< whenElt

build :: forall a. ((a -> Array a -> Array a) -> Array a -> Array a) -> Array a
build g = g (\x xs -> x : xs) []

chunksOf :: forall e. Int -> Array e -> Array (Array e)
chunksOf i ls = map (take i) (build (splitter ls)) 
  where
    splitter :: forall a. Array e -> (Array e -> a -> a) -> a -> a
    splitter l c n = case uncons l of
      Nothing -> n
      Just _ -> l `c` splitter (drop i l) c n

splitAt :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt n xs = { before: take n xs, after: drop n xs }

splitPlaces :: forall e. Array Int -> Array e -> Array (Array e)
splitPlaces is ys = build (splitPlacer is ys)
  where
    splitPlacer :: forall b t. Array Int -> Array b -> (Array b -> t -> t) -> t -> t
    splitPlacer xss yss c n = case uncons xss, uncons yss of
      Nothing, _ -> n
      _, Nothing -> n
      Just {head, tail}, _ -> let {before, after} = splitAt head yss in before `c` splitPlacer tail after c n

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

-- | Divides up an input array into a set of subarrays, according to 'n' and 'm'
-- | input specifications you provide. Each subarray will have 'n' items, and the
-- | start of each subarray will be offset by 'm' items from the previous one.
-- | ```purescript
-- | divvy 5 5 (1..20) == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
-- | ```

divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
divvy n m l = case uncons l of
  Nothing -> []
  Just _ -> filter (\ws -> (n == length ws)) choppedl
    where choppedl = chop (\xs -> { left: take n xs , right: drop m xs }) l
