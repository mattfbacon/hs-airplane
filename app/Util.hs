module Util(showTriple, showPair) where

showTriple :: Show a => String -> (a, a, a) -> String
showTriple delim (a, b, c) = show a ++ delim ++ show b ++ delim ++ show c

showPair :: Show a => String -> (a, a) -> String
showPair delim (a, b) = show a ++ delim ++ show b
