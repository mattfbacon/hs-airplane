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

data MatchResult a
  = NoMatch String [String]
  | Match a
  | AmbiguousMatch String [String]

instance Functor MatchResult where
  fmap _ (NoMatch name choices) = NoMatch name choices
  fmap fn (Match m) = Match $ fn m
  fmap _ (AmbiguousMatch name ms) = AmbiguousMatch name ms

instance Applicative MatchResult where
  pure = return
  (<*>) = ap

instance Monad MatchResult where
  NoMatch name choices >>= _ = NoMatch name choices
  Match m >>= f = f m
  AmbiguousMatch name ms >>= _ = AmbiguousMatch name ms
  return = Match

-- fail _ = NoMatch

matcher :: String -> [String] -> String -> MatchResult String
matcher name possibilities given =
  let matches = L.filter (given `L.isPrefixOf`) possibilities
   in case L.length matches of
        0 -> NoMatch name possibilities
        1 -> Match $ L.head matches
        _ -> AmbiguousMatch name matches

matchAll :: (String -> MatchResult a) -> [String] -> MatchResult [a]
matchAll = go []
  where
    go :: [a] -> (String -> MatchResult a) -> [String] -> MatchResult [a]
    go matchesSoFar _ [] = Match matchesSoFar -- consolidate all matches into a single MatchResult
    go matchesSoFar fn (given : givens) -- using MatchResult monad ;)
      =
      do
        m <- fn given
        go (m : matchesSoFar) fn givens

userMessage :: MatchResult a -> Either String a
userMessage match_ =
  case match_ of
    NoMatch name choices -> Left $ titleCase name ++ " can be " ++ L.intercalate ", " choices
    Match match -> Right match
    AmbiguousMatch name options -> Left $ "For " ++ titleCase name ++ ", please specify between " ++ L.intercalate ", " options
