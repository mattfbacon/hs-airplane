module Plane where

import Data.List qualified as L
import Data.Set qualified as Set
import Util (alphaNum, replaceIdx, showPair)

letters :: [Char]
letters = enumFromTo 'a' 'z'

data SeatState = Empty | Full deriving (Eq)
instance Show SeatState where
  show Empty = "o"
  show Full = "x"

data SeatArea = Economy | FirstClass
seatAreas :: [String]
seatAreas = ["economy", "firstClass"]
stringToSeatArea :: String -> SeatArea
stringToSeatArea "economy" = Economy
stringToSeatArea "firstClass" = FirstClass

data SeatSide = LeftSide | RightSide
seatSides :: [String]
seatSides = ["left", "right"]
stringToSeatSide :: String -> SeatSide
stringToSeatSide "left" = LeftSide
stringToSeatSide "right" = RightSide

data EconomySeatColumn = EAisle | EMiddle | EWindow
economySeatColumns :: [String]
economySeatColumns = ["aisle", "middle", "window"]
stringToEconomySeatColumn :: String -> EconomySeatColumn
stringToEconomySeatColumn "aisle" = EAisle
stringToEconomySeatColumn "middle" = EMiddle
stringToEconomySeatColumn "window" = EWindow

data FirstClassSeatColumn = FCAisle | FCWindow
firstClassSeatColumns :: [String]
firstClassSeatColumns = ["aisle", "window"]
stringToFirstClassSeatColumn :: String -> FirstClassSeatColumn
stringToFirstClassSeatColumn "aisle" = FCAisle
stringToFirstClassSeatColumn "window" = FCWindow

class TwoSided a b where
  getSide :: SeatSide -> a -> b
class GetSeatable seatIdx seatSide where
  getSeat :: seatIdx -> seatSide -> SeatState

data EconomySide = EconomySide
  { eSidesSide :: SeatSide
  , eAisle :: SeatState
  , eMiddle :: SeatState
  , eWindow :: SeatState
  }
instance GetSeatable EconomySeatColumn EconomySide where
  getSeat EAisle side = eAisle side
  getSeat EMiddle side = eMiddle side
  getSeat EWindow side = eWindow side

newtype EconomyRow = EconomyRow (EconomySide, EconomySide)
instance TwoSided EconomyRow EconomySide where
  getSide LeftSide (EconomyRow (left, _)) = left
  getSide RightSide (EconomyRow (_, right)) = right

instance Show EconomySide where
  show (EconomySide sidesSide aisle middle window) = case sidesSide of
    LeftSide -> show window ++ show middle ++ show aisle
    RightSide -> show aisle ++ show middle ++ show window
instance Show EconomyRow where
  show (EconomyRow row) = showPair "||" row

data FirstClassSide = FirstClassSide
  { fcSidesSide :: SeatSide
  , fcAisle :: SeatState
  , fcWindow :: SeatState
  }
instance GetSeatable FirstClassSeatColumn FirstClassSide where
  getSeat FCAisle side = fcAisle side
  getSeat FCWindow side = fcWindow side

newtype FirstClassRow = FirstClassRow (FirstClassSide, FirstClassSide)
instance TwoSided FirstClassRow FirstClassSide where
  getSide LeftSide (FirstClassRow (left, _)) = left
  getSide RightSide (FirstClassRow (_, right)) = right

instance Show FirstClassSide where
  show (FirstClassSide sidesSide aisle window) = case sidesSide of
    LeftSide -> show window ++ " " ++ show aisle
    RightSide -> show aisle ++ " " ++ show window
instance Show FirstClassRow where
  show (FirstClassRow row) = showPair "||" row

newtype EconomySection = EconomySection [EconomyRow]
newtype FirstClassSection = FirstClassSection [FirstClassRow]

data Plane = Plane
  { economy :: EconomySection
  , firstClass :: FirstClassSection
  }

emptyEconomySide :: SeatSide -> EconomySide
emptyEconomySide side = EconomySide side Empty Empty Empty
emptyEconomyRow :: EconomyRow
emptyEconomyRow = EconomyRow (emptyEconomySide LeftSide, emptyEconomySide RightSide)

emptyFirstClassSide :: SeatSide -> FirstClassSide
emptyFirstClassSide side = FirstClassSide side Empty Empty
emptyFirstClassRow :: FirstClassRow
emptyFirstClassRow = FirstClassRow (emptyFirstClassSide LeftSide, emptyFirstClassSide RightSide)

emptyPlane :: Int -> Int -> Plane
emptyPlane economyRows firstClassRows = Plane (EconomySection $ replicate economyRows emptyEconomyRow) (FirstClassSection $ replicate firstClassRows emptyFirstClassRow)

showFirstClass :: Plane -> String
showFirstClass (Plane _ (FirstClassSection firstClass)) = L.intercalate "\n" $
  [ "First Class"
  , " |1 2||3 4|"
  ] ++ zipWith (\letter row -> letter : '|' : show row ++ "|") letters firstClass

showEconomy :: Plane -> String
showEconomy (Plane (EconomySection economy) _) = L.intercalate "\n" $
  [ "Economy"
  , " |123||456|"
  ] ++ zipWith (\letter row -> letter : '|' : show row ++ "|") letters economy

instance Show Plane where
  show plane = showFirstClass plane ++ "\n" ++ showEconomy plane
