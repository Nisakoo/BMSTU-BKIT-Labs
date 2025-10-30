module Main

import System.REPL
import Data.String


average : (str : String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLength (words str)) in
                  cast totalLength / cast numWords
    where
        wordCount : String -> Nat
        wordCount strs = length (words str)

        allLength : List String -> List Nat
        allLength strs = map length strs


showAverage : String -> String
showAverage str = show (average str) ++ "\n"


main : IO()
main = repl "" showAverage