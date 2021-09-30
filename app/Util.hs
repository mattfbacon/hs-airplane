module Util
  ( showTriple,
    showPair,
    alphaNum,
    replaceIdx,
    titleCase,
  )
where

import Data.List (unfoldr)
import GHC.Unicode (toLower, toUpper)

showTriple :: Show aT => Show bT => Show cT => String -> (aT, bT, cT) -> String
showTriple delim (a, b, c) = show a ++ delim ++ show b ++ delim ++ show c

showPair :: Show aT => Show bT => String -> (aT, bT) -> String
showPair delim (a, b) = show a ++ delim ++ show b

alpha :: [Char]
alpha = ['a' .. 'z']

alphaNum :: Int -> String
alphaNum =
  reverse
    . unfoldr
      ( \case
          Nothing -> Nothing
          Just x ->
            let a = quotRem x 26
             in Just
                  ( alpha !! snd a,
                    case fst a of
                      0 -> Nothing
                      x -> Just $ x - 1
                  )
      )
    . Just

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx idx newValue list =
  case list of
    [] -> []
    (x : xs) ->
      if idx == 0 -- this is the item to replace
        then newValue : xs
        else x : replaceIdx (idx - 1) newValue xs

titleCase :: String -> String
titleCase "" = ""
titleCase (char : chars) = toUpper char : map toLower chars
