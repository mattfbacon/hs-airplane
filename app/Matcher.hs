module Matcher(matcher, MatchResult(..)) where

import qualified Data.List as L
import qualified Data.List.Split as S

data MatchResult = NoMatch | Match String | AmbiguousMatch [String]

matcher :: [String] -> String -> MatchResult
matcher possibilities given =
  let matches = filter (given `L.isPrefixOf`) possibilities
  in case length matches of
    0 -> NoMatch
    1 -> Match $ head matches
    _ -> AmbiguousMatch matches
