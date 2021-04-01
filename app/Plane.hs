module Plane( SeatState(..), Plane, emptyPlane ) where

import qualified Data.List as L

import TupleShow ( showTriple, showPair )

letters = enumFromTo 'a' 'z'

data SeatState = Empty | Full deriving (Eq)
instance Show SeatState where
  show Empty = "o"
  show Full = "x"

newtype EconomySide = EconomySide (SeatState, SeatState, SeatState) deriving (Eq)
newtype EconomyRow = EconomyRow (EconomySide, EconomySide) deriving (Eq)

instance Show EconomySide where
  show (EconomySide side) = showTriple "" side
instance Show EconomyRow where
  show (EconomyRow row) = showPair "||" row

newtype FirstClassSide = FirstClassSide (SeatState, SeatState) deriving (Eq)
newtype FirstClassRow = FirstClassRow (FirstClassSide, FirstClassSide) deriving (Eq)

instance Show FirstClassSide where
  show (FirstClassSide side) = showPair " " side
instance Show FirstClassRow where
  show (FirstClassRow row) = showPair "||" row

newtype EconomySection = EconomySection [EconomyRow] deriving (Eq)
newtype FirstClassSection = FirstClassSection [FirstClassRow] deriving (Eq)

data Plane = Plane
  { economy :: EconomySection
  , firstClass :: FirstClassSection
  }

emptyEconomySide :: EconomySide
emptyEconomySide = EconomySide (Empty, Empty, Empty)
emptyEconomyRow :: EconomyRow
emptyEconomyRow = EconomyRow (emptyEconomySide, emptyEconomySide)

emptyFirstClassSide = FirstClassSide (Empty, Empty)
emptyFirstClassRow = FirstClassRow (emptyFirstClassSide, emptyFirstClassSide)

emptyPlane :: Plane
emptyPlane = Plane (EconomySection $ replicate 15 emptyEconomyRow) (FirstClassSection $ replicate 5 emptyFirstClassRow)

instance Show Plane where
  show (Plane (EconomySection economy) (FirstClassSection firstClass)) = L.intercalate "\n"
    [ "First Class"
    , " |1 2||3 4|"
    , L.intercalate "\n" $ zipWith (\letter row -> letter : '|' : show row ++ "|") letters firstClass
    , "Economy"
    , " |123||456|"
    , L.intercalate "\n" $ zipWith (\letter row -> letter : '|' : show row ++ "|") letters economy
    ]
