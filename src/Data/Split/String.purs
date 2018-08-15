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

type SplitterChar = SplitterArray Char

split :: SplitterChar -> String -> Array String
split s = map S.fromCharArray <<< SA.split s <<< S.toCharArray

oneOf :: String -> SplitterChar
oneOf = SA.oneOf <<< S.toCharArray

onSublist :: String -> SplitterChar
onSublist = SA.onSublist <<< S.toCharArray

whenElt :: (Char -> Boolean) -> SplitterChar
whenElt = SA.whenElt

dropDelims :: SplitterChar -> SplitterChar
dropDelims = SA.dropDelims

keepDelimsL :: SplitterChar -> SplitterChar
keepDelimsL = SA.keepDelimsL

keepDelimsR :: SplitterChar -> SplitterChar
keepDelimsR = SA.keepDelimsR

condense :: SplitterChar -> SplitterChar
condense = SA.condense

dropInitBlank :: SplitterChar -> SplitterChar
dropInitBlank = SA.dropInitBlank

dropFinalBlank :: SplitterChar -> SplitterChar
dropFinalBlank = SA.dropFinalBlank

dropInnerBlanks :: SplitterChar -> SplitterChar
dropInnerBlanks = SA.dropInnerBlanks

dropBlanks :: SplitterChar -> SplitterChar
dropBlanks = SA.dropBlanks

startsWith :: String -> SplitterChar
startsWith = dropInitBlank <<< keepDelimsL <<< onSublist

startsWithOneOf :: String -> SplitterChar
startsWithOneOf = dropInitBlank <<< keepDelimsL <<< oneOf

endsWith :: String -> SplitterChar
endsWith = dropFinalBlank <<< keepDelimsR <<< onSublist

endsWithOneOf :: String -> SplitterChar
endsWithOneOf = dropFinalBlank <<< keepDelimsR <<< oneOf

splitOneOf :: String -> String -> Array String
splitOneOf = split <<< dropDelims <<< oneOf

splitOn :: String -> String -> Array String
splitOn = split <<< dropDelims <<< onSublist

splitWhen :: (Char -> Boolean) -> String -> Array String
splitWhen = split <<< dropDelims <<< whenElt

endBy :: String -> String -> Array (String)
endBy = split <<< dropFinalBlank <<< dropDelims <<< onSublist

endByOneOf :: String -> String -> Array (String)
endByOneOf = split <<< dropFinalBlank <<< dropDelims <<< oneOf

wordsBy :: (Char -> Boolean) -> String -> Array (String)
wordsBy = split <<< dropBlanks <<< dropDelims <<< whenElt

linesBy :: (Char -> Boolean) -> String -> Array (String)
linesBy = split <<< dropFinalBlank <<< dropDelims <<< whenElt

chunksOf :: Int -> String -> Array String
chunksOf i ls = map S.fromCharArray $ SA.chunksOf i (S.toCharArray ls)

splitPlaces :: Array Int -> String -> Array String
splitPlaces is ys = map S.fromCharArray $ SA.splitPlaces is (S.toCharArray ys)

splitPlacesBlanks :: Array Int -> String -> Array String
splitPlacesBlanks is ys = map S.fromCharArray $ SA.splitPlacesBlanks is (S.toCharArray ys)

divvy :: Int -> Int -> String -> Array String
divvy n m s = map S.fromCharArray $ SA.divvy n m (S.toCharArray s)