module Test.Main where

import Prelude

import Data.Array ((..))
import Data.Split.Array as SA
import Data.Split.String as SS
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow $ SA.splitPlaces [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
  logShow $ SA.splitPlaces [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
  logShow $ SA.splitPlaces [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
  logShow $ SA.splitPlacesBlanks [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
  logShow $ SA.splitPlacesBlanks [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
  logShow $ SA.splitPlacesBlanks [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10],[]]
  logShow $ SS.splitPlaces [2,3,4] "abcdefghjk" == ["ab","cde","fghj"]
  logShow $ SS.splitPlacesBlanks [4,9,3] "abcdefghjk" == ["abcd","efghjk",""]
  logShow $ SA.divvy 5 5 (1..20) == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
  logShow $ SA.divvy 5 2 (1..10) == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
  logShow $ SS.divvy 5 2 "abcdefghjk" == ["abcde","cdefg","efghj"]
  logShow $ SS.splitOn "x" "axbxc" == ["a","b","c"]
  logShow $ SS.splitOn "x" "axbxcx" == ["a","b","c",""]
  logShow $ SS.endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
  logShow $ SS.splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
  logShow $ SS.splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
  logShow $ SS.endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
  logShow $ SS.wordsBy (_=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
  logShow $ SS.linesBy (_=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
  logShow $ SS.split (SS.oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
  logShow $ SS.split (SS.onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
  logShow $ SS.split (SS.onSublist "") "abc" == ["","","a","","b","","c"]
  logShow $ SS.split (SS.dropDelims <<< SS.dropBlanks $ SS.onSublist "") "abc" == ["a","b","c"]
  logShow $ SS.split (SS.oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
  logShow $ SS.split (SS.dropDelims $ SS.oneOf ":") "a:b:c" == ["a", "b", "c"]
  logShow $ SS.split (SS.keepDelimsL $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
  logShow $ SS.split (SS.keepDelimsR $ SS.oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
  logShow $ SS.split (SS.condense $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","xyz","c","x","d"]

  logShow $ SS.split (SS.dropDelims $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","b","","","c","d"]
  logShow $ SS.split (SS.condense <<< SS.dropDelims $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","b","c","d"]
  logShow $ SS.split (SS.oneOf ":") ":a:b" == ["",":","a",":","b"]
  logShow $ SS.split (SS.dropInitBlank $ SS.oneOf ":") ":a:b" == [":","a",":","b"]
  logShow $ SS.split (SS.oneOf ":") "a:b:" == ["a",":","b",":",""]

  logShow $ SS.split (SS.dropFinalBlank $ SS.oneOf ":") "a:b:" == ["a",":","b",":"]
  logShow $ SS.split (SS.oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
  logShow $ SS.split (SS.dropInnerBlanks $ SS.oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
  logShow $ SS.split (SS.oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
  logShow $ SS.split (SS.dropBlanks $ SS.oneOf ":") "::b:::a" == ["::","b",":::","a"]
  logShow $ SS.split (SS.startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
  -- split (startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
  logShow $ SS.split (SS.endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
  logShow $ SS.split (SS.condense $ SS.endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
  logShow $ SS.split (SS.oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
  logShow $ SS.split (SS.onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
  logShow $ SS.split (SS.onSublist "") "abc" == ["","","a","","b","","c"]
  logShow $ SS.split (SS.dropDelims <<< SS.dropBlanks $ SS.onSublist "") "abc" == ["a","b","c"]
  logShow $ SA.split (SA.whenElt (_<0)) [2,4,-3,6,-9,1] == [[2,4],[-3],[6],[-9],[1]]
  logShow $ SS.split (SS.oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
  logShow $ SS.split (SS.dropDelims $ SS.oneOf ":") "a:b:c" == ["a", "b", "c"]
  logShow $ SS.split (SS.keepDelimsL $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
  logShow $ SS.split (SS.keepDelimsR $ SS.oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
  logShow $ SS.split (SS.condense $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","xyz","c","x","d"]
  logShow $ SS.split (SS.dropDelims $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","b","","","c","d"]
  logShow $ SS.split (SS.condense <<< SS.dropDelims $ SS.oneOf "xyz") "aazbxyzcxd" == ["aa","b","c","d"]
  logShow $ SS.split (SS.oneOf ":") ":a:b" == ["",":","a",":","b"]
  logShow $ SS.split (SS.dropInitBlank $ SS.oneOf ":") ":a:b" == [":","a",":","b"]
  logShow $ SS.split (SS.oneOf ":") "a:b:" == ["a",":","b",":",""]
  logShow $ SS.split (SS.dropFinalBlank $ SS.oneOf ":") "a:b:" == ["a",":","b",":"]
  logShow $ SS.split (SS.oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
  logShow $ SS.split (SS.dropInnerBlanks $ SS.oneOf ":") "::b:::a" == ["", ":",":","b",":",":",":","a"]
  logShow $ SS.split (SS.oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
  logShow $ SS.split (SS.dropBlanks $ SS.oneOf ":") "::b:::a" == ["::","b",":::","a"]
  logShow $ SS.split (SS.startsWith "app") "applyapplicativeapplaudapproachapple" == ["apply","applicative","applaud","approach","apple"]
  -- logShow $ SS.split (SS.startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
  logShow $ SS.split (SS.endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
  logShow $ SS.split (SS.condense $ SS.endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
  logShow $ SS.splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
  logShow $ SS.splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
  logShow $ SA.splitWhen (_<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
  logShow $ SS.endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
  logShow $ SS.endByOneOf ";," "foo;bar,baz;" == ["foo","bar","baz"]
  logShow $ SS.wordsBy (_=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
  logShow $ SS.linesBy (_=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
  logShow $ SA.splitPlaces [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
  logShow $ SA.splitPlaces [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
  logShow $ SA.splitPlaces [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
  logShow $ SA.splitPlacesBlanks [2,3,4] (1..20) == [[1,2],[3,4,5],[6,7,8,9]]
  logShow $ SA.splitPlacesBlanks [4,9] (1..10) == [[1,2,3,4],[5,6,7,8,9,10]]
  logShow $ SA.splitPlacesBlanks [4,9,3] (1..10) == [[1,2,3,4],[5,6,7,8,9,10],[]]








