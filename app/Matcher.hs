module Matcher
  ( matcher,
    MatchResult (..),
    matchAll,
    userMessage,
  )
where

import Control.Monad (ap)
import Data.List qualified as L
import Util (titleCase)

data MatchResult = NoMatch | Match String | AmbiguousMatch [String]

matcher :: [String] -> String -> MatchResult
matcher possibilities given =
  let matches = filter (given `L.isPrefixOf`) possibilities
  in case length matches of
    0 -> NoMatch
    1 -> Match $ head matches
    _ -> AmbiguousMatch matches
