module Data.Split.List
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
    , SplitterList
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

import Data.Common (Chunk(..), CondensePolicy(..), DelimPolicy(..), Delimiter(..), EndPolicy(..), Splitter, fromElem, isDelim, isText)
import Data.Foldable (elem, null)
import Data.List (List(..), concatMap, drop, filter, length, singleton, span, take, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type DelimiterList = Delimiter List
type ChunkList = Chunk List
type SplitList a = List (ChunkList a)
type SplitterList a = Splitter List a

defaultSplitter :: forall a. SplitterList a
defaultSplitter 
  = { delimiter: Delimiter (singleton (const false))
    , delimPolicy: Keep
    , condensePolicy: KeepBlankFields
    , initBlankPolicy: KeepBlank
    , finalBlankPolicy: KeepBlank }

matchDelim :: forall a. DelimiterList a -> List a -> Maybe { left :: List a, right :: List a }
matchDelim (Delimiter Nil) xs = Just { left: Nil, right: xs }
matchDelim (Delimiter _)  Nil = Nothing
matchDelim (Delimiter (p : ps)) (x : xs)
  | p x = matchDelim (Delimiter ps) xs >>= \t -> Just { left: x : t.left, right: t.right }
  | otherwise = Nothing

splitInternal :: forall a. DelimiterList a -> List a -> SplitList a
splitInternal _ Nil = Nil
splitInternal d xxs = let Tuple xs match = breakDelim d xxs in
  if null xs
    then
      toSplitList match
    else
      Text xs : toSplitList match
    where 
      toSplitList Nothing = Nil
      toSplitList (Just ({ left: Nil, right: r : rs })) = (Delim Nil) : Text (singleton r) : splitInternal d rs
      toSplitList (Just ({ left, right })) = Delim left : splitInternal d right

breakDelim :: forall a. DelimiterList a -> List a -> Tuple (List a) (Maybe { left :: List a, right :: List a })
breakDelim (Delimiter Nil) xs = Tuple Nil (Just { left: Nil, right: xs })
breakDelim _ Nil = Tuple Nil Nothing
breakDelim d xxs@(x : xs) =
  case matchDelim d xxs of
    Nothing -> let (Tuple ys match) = breakDelim d xs in Tuple (x : ys) match
    Just match -> Tuple Nil (Just match)

doDrop :: forall a. DelimPolicy -> SplitList a -> SplitList a
doDrop Drop l = filter isText l
doDrop _ l = l

doCondense :: forall a. CondensePolicy -> SplitList a -> SplitList a
doCondense Condense ls = condense' ls
  where 
    condense' Nil = Nil
    condense' (c@(Text _) : l) = c : condense' l
    condense' l = (Delim $ concatMap fromElem s.init) : condense' s.rest
      where 
        s = span isDelim l
doCondense _ ls = ls

insertBlanks' :: forall a. CondensePolicy -> SplitList a -> SplitList a
insertBlanks' _ Nil = Nil
insertBlanks' cp@(DropBlankFields) (d1@(Delim _) : d2@(Delim _) : l) = d1 : insertBlanks' cp (d2 : l)
insertBlanks' cp (d1@(Delim _) : d2@(Delim _) : l) = d1 : Text Nil : insertBlanks' cp (d2 : l)
insertBlanks' _ (d@(Delim _) : Nil) = d : Text Nil : Nil
insertBlanks' cp (c : l) = c : insertBlanks' cp l

insertBlanks :: forall a. CondensePolicy -> SplitList a -> SplitList a
insertBlanks _ Nil = Text Nil : Nil
insertBlanks cp (d@(Delim _) : l) = Text Nil : insertBlanks' cp (d : l)
insertBlanks cp l = insertBlanks' cp l

doMerge :: forall a. DelimPolicy -> SplitList a -> SplitList a
doMerge KeepLeft = mergeLeft
doMerge KeepRight = mergeRight
doMerge _ = identity

mergeLeft :: forall a. SplitList a -> SplitList a
mergeLeft Nil = Nil
mergeLeft ((Delim d) : (Text c) : l) = Text (d <> c) : mergeLeft l
mergeLeft (c : l) = c : mergeLeft l

mergeRight :: forall a. SplitList a -> SplitList a
mergeRight Nil = Nil
mergeRight ((Text c) : l) = Text (c <> d) : mergeRight lTail
  where 
    Tuple d lTail = case l of
                        (Delim d') : l' -> Tuple d' l'
                        _ -> Tuple Nil l
mergeRight (c : l) = c : mergeRight l

dropInitial :: forall a. EndPolicy -> SplitList a -> SplitList a
dropInitial DropBlank ((Text Nil) : l) = l
dropInitial _ l = l

dropFinal :: forall a. EndPolicy -> SplitList a -> SplitList a
dropFinal _ Nil = Nil
dropFinal DropBlank l  = dropFinal' l
  where 
    dropFinal' Nil = Nil
    dropFinal' ((Text Nil) : Nil) = Nil
    dropFinal' (x : xs) = x : dropFinal' xs
dropFinal _ l  = l

postProcess :: forall a. SplitterList a -> SplitList a -> SplitList a
postProcess s 
  = dropFinal (s.finalBlankPolicy) 
  <<< dropInitial (s.initBlankPolicy) 
  <<< doMerge (s.delimPolicy) 
  <<< doDrop (s.delimPolicy) 
  <<< insertBlanks (s.condensePolicy) 
  <<< doCondense (s.condensePolicy)

split :: forall a. SplitterList a -> List a -> List (List a)
split s = map fromElem <<< postProcess s <<< splitInternal (s.delimiter)

oneOf :: forall a. Eq a => List a -> SplitterList a
oneOf elts = defaultSplitter { delimiter = Delimiter (singleton (\x -> x `elem` elts)) }

onSublist :: forall a. Eq a => List a -> SplitterList a
onSublist lst = defaultSplitter { delimiter = Delimiter (map (==) lst) }

whenElt :: forall a. (a -> Boolean) -> SplitterList a
whenElt p = defaultSplitter { delimiter = Delimiter (singleton p) }

keepDelimsL :: forall a. SplitterList a -> SplitterList a
keepDelimsL s = s { delimPolicy = KeepLeft }

dropDelims :: forall a. SplitterList a -> SplitterList a
dropDelims s = s { delimPolicy = Drop }

keepDelimsR :: forall a. SplitterList a -> SplitterList a
keepDelimsR s = s { delimPolicy = KeepRight }

condense :: forall a. SplitterList a -> SplitterList a
condense s = s { condensePolicy = Condense }

dropInitBlank :: forall a. SplitterList a -> SplitterList a
dropInitBlank s = s { initBlankPolicy = DropBlank }

dropFinalBlank :: forall a. SplitterList a -> SplitterList a
dropFinalBlank s = s { finalBlankPolicy = DropBlank }

dropInnerBlanks :: forall a. SplitterList a -> SplitterList a
dropInnerBlanks s = s { condensePolicy = DropBlankFields }

dropBlanks :: forall a. SplitterList a -> SplitterList a
dropBlanks = dropInitBlank <<< dropFinalBlank <<< condense

startsWith :: forall a. Eq a => List a -> SplitterList a
startsWith = dropInitBlank <<< keepDelimsL <<< onSublist

startsWithOneOf :: forall a. Eq a => List a -> SplitterList a
startsWithOneOf = dropInitBlank <<< keepDelimsL <<< oneOf

endsWith :: forall a. Eq a => List a -> SplitterList a
endsWith = dropFinalBlank <<< keepDelimsR <<< onSublist

endsWithOneOf :: forall a. Eq a => List a -> SplitterList a
endsWithOneOf = dropFinalBlank <<< keepDelimsR <<< oneOf

splitOneOf :: forall a. Eq a => List a -> List a -> List (List  a)
splitOneOf = split <<< dropDelims <<< oneOf

splitOn :: forall a. Eq a => List a -> List a -> List (List  a)
splitOn = split <<< dropDelims <<< onSublist

splitWhen :: forall a. (a -> Boolean) -> List a -> List (List a)
splitWhen = split <<< dropDelims <<< whenElt

endBy :: forall a. Eq a => List a -> List a -> List (List a)
endBy = split <<< dropFinalBlank <<< dropDelims <<< onSublist

endByOneOf :: forall a. Eq a => List a -> List a -> List (List a)
endByOneOf = split <<< dropFinalBlank <<< dropDelims <<< oneOf

wordsBy :: forall a. (a -> Boolean) -> List a -> List (List a)
wordsBy = split <<< dropBlanks <<< dropDelims <<< whenElt

linesBy :: forall a. (a -> Boolean) -> List a -> List (List a)
linesBy = split <<< dropFinalBlank <<< dropDelims <<< whenElt

build :: forall a. ((a -> List a -> List a) -> List a -> List a) -> List a
build g = g (:) Nil

chunksOf :: forall e. Int -> List e -> List (List e)
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: forall a. List e -> (List e -> a -> a) -> a -> a
  splitter Nil _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

splitAt :: forall a. Int -> List a -> { before :: List a, after :: List a }
splitAt n xs = { before: take n xs, after: drop n xs }

splitPlaces :: forall e. List Int -> List e -> List (List e)
splitPlaces is ys = build (splitPlacer is ys) where
  splitPlacer :: forall b t. List Int -> List b -> (List b -> t -> t) -> t -> t
  splitPlacer Nil _ _ n = n
  splitPlacer _ Nil _ n = n
  splitPlacer (l:ls) xs c n = let {before, after} = splitAt l xs in before `c` splitPlacer ls after c n

splitPlacesBlanks :: forall e. List Int -> List e -> List (List e)
splitPlacesBlanks is ys = build (splitPlacer is ys) where
  splitPlacer :: forall b t. List Int -> List b -> (List b -> t -> t) -> t -> t
  splitPlacer Nil _ _ n = n
  splitPlacer (l:ls) xs c n = let {before, after} = splitAt l xs in before `c` splitPlacer ls after c n

chop :: forall a b. (List a -> { left:: b, right :: List a }) -> List a -> List b
chop _ Nil = Nil
chop f as = let r = f as in r.left : chop f r.right

divvy :: forall a. Int -> Int -> List a -> List (List a)
divvy _ _ Nil = Nil
divvy n m l = filter (\ws -> (n == length ws)) choppedl
  where choppedl = chop (\xs -> { left: take n xs , right: drop m xs }) l