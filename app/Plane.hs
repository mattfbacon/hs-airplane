module Plane where

import Data.List qualified as L
import Data.Set qualified as Set
import Util (alphaNum, replaceIdx, showPair)

letters :: [Char]
letters = enumFromTo 'a' 'z'

data SeatState
  = Empty
  | Full
  deriving (Eq)

instance Show SeatState where
  show Empty = "o"
  show Full = "x"

seatStateHumanReadable :: SeatState -> String
seatStateHumanReadable Empty = "Empty"
seatStateHumanReadable Full = "Full"

data SeatArea
  = Economy
  | FirstClass
  deriving (Eq)

seatAreas :: [String]
seatAreas = ["economy", "firstClass"]

stringToSeatArea :: String -> SeatArea
stringToSeatArea "economy" = Economy
stringToSeatArea "firstClass" = FirstClass
stringToSeatArea _ = error "String can't be converted to SeatArea"

seatAreaSeats :: SeatArea -> Int
seatAreaSeats Economy = 3
seatAreaSeats FirstClass = 2

data SeatSide
  = LeftSide
  | RightSide
  deriving (Eq)

instance Show SeatSide where
  show LeftSide = "left"
  show RightSide = "right"

seatSides :: [String]
seatSides = ["left", "right"]

stringToSeatSide :: String -> SeatSide
stringToSeatSide "left" = LeftSide
stringToSeatSide "right" = RightSide
stringToSeatSide _ = error "String can't be converted to SeatSide"

data EconomySeatColumn
  = EAisle
  | EMiddle
  | EWindow
  deriving (Eq, Ord)

economySeatColumns :: [String]
economySeatColumns = ["aisle", "middle", "window"]

stringToEconomySeatColumn :: String -> EconomySeatColumn
stringToEconomySeatColumn "aisle" = EAisle
stringToEconomySeatColumn "middle" = EMiddle
stringToEconomySeatColumn "window" = EWindow
stringToEconomySeatColumn _ = error "String can't be converted to EconomySeatColumn"

data FirstClassSeatColumn
  = FCAisle
  | FCWindow
  deriving (Eq, Ord)

firstClassSeatColumns :: [String]
firstClassSeatColumns = ["aisle", "window"]

stringToFirstClassSeatColumn :: String -> FirstClassSeatColumn
stringToFirstClassSeatColumn "aisle" = FCAisle
stringToFirstClassSeatColumn "window" = FCWindow
stringToFirstClassSeatColumn _ = error "String can't be converted to FirstClassSeatColumn"

class
  IsSide b =>
  TwoSided a b
    | a -> b
  where
  getSide :: SeatSide -> a -> b
  replaceSide :: b -> a -> a

class
  IsSide seatSide =>
  GetSeatable seatIdx seatSide
  where
  getSeat :: seatIdx -> seatSide -> SeatState

class IsSide seatSide where
  numFreeSeats :: seatSide -> Int
  describe :: seatSide -> String
  sidesRow :: seatSide -> Int
  sidesSide :: seatSide -> SeatSide
  sidesArea :: seatSide -> SeatArea
  makeSide :: SeatState -> Int -> SeatSide -> seatSide
  updatePlaneWithNew :: seatSide -> seatSide -> Plane -> Plane

data EconomySide = EconomySide
  { eSidesRow :: Int,
    eSidesSide :: SeatSide,
    eAisle :: SeatState,
    eMiddle :: SeatState,
    eWindow :: SeatState
  }
  deriving (Eq)

instance GetSeatable EconomySeatColumn EconomySide where
  getSeat EAisle side = eAisle side
  getSeat EMiddle side = eMiddle side
  getSeat EWindow side = eWindow side

instance IsSide EconomySide where
  numFreeSeats side = length $ filter (\seat -> getSeat seat side == Empty) [EAisle, EMiddle, EWindow]
  describe side = "Row " ++ show (eSidesRow side) ++ " on the " ++ show (eSidesSide side)
  sidesRow = eSidesRow
  sidesSide = eSidesSide
  sidesArea _ = Economy
  makeSide state row side = EconomySide row side state state state
  updatePlaneWithNew oldSide newSide (Plane (EconomySection econ) fc) =
    Plane (EconomySection $ replaceIdx (sidesRow oldSide) (replaceSide newSide (econ !! sidesRow oldSide)) econ) fc

data EconomyRow
  = EconomyRow Int (EconomySide, EconomySide)
  deriving (Eq)

instance TwoSided EconomyRow EconomySide where
  getSide LeftSide (EconomyRow _ (left, _)) = left
  getSide RightSide (EconomyRow _ (_, right)) = right
  replaceSide newSide (EconomyRow rowNum (left, right)) =
    case sidesSide newSide of
      LeftSide -> EconomyRow rowNum (newSide, right)
      RightSide -> EconomyRow rowNum (left, newSide)

instance Show EconomySide where
  show (EconomySide _ sidesSide aisle middle window) =
    case sidesSide of
      LeftSide -> show window ++ show middle ++ show aisle
      RightSide -> show aisle ++ show middle ++ show window

instance Show EconomyRow where
  show (EconomyRow rowNum row) = alphaNum (rowNum - 1) ++ "|" ++ showPair "||" row ++ "|"

data FirstClassSide = FirstClassSide
  { fcSidesRow :: Int,
    fcSidesSide :: SeatSide,
    fcAisle :: SeatState,
    fcWindow :: SeatState
  }
  deriving (Eq)

instance GetSeatable FirstClassSeatColumn FirstClassSide where
  getSeat FCAisle side = fcAisle side
  getSeat FCWindow side = fcWindow side

instance IsSide FirstClassSide where
  numFreeSeats side = length $ filter (\seat -> getSeat seat side == Empty) [FCAisle, FCWindow]
  describe side = "Row " ++ show (fcSidesRow side) ++ " on the " ++ show (fcSidesSide side)
  sidesRow = fcSidesRow
  sidesSide = fcSidesSide
  sidesArea _ = FirstClass
  makeSide state row side = FirstClassSide row side state state
  updatePlaneWithNew oldSide newSide (Plane econ (FirstClassSection fc)) =
    Plane econ (FirstClassSection $ replaceIdx (sidesRow oldSide) (replaceSide newSide (fc !! sidesRow oldSide)) fc)

data FirstClassRow
  = FirstClassRow Int (FirstClassSide, FirstClassSide)
  deriving (Eq)

instance TwoSided FirstClassRow FirstClassSide where
  getSide LeftSide (FirstClassRow _ (left, _)) = left
  getSide RightSide (FirstClassRow _ (_, right)) = right
  replaceSide newSide (FirstClassRow rowNum (left, right)) =
    case sidesSide newSide of
      LeftSide -> FirstClassRow rowNum (newSide, right)
      RightSide -> FirstClassRow rowNum (left, newSide)

instance Show FirstClassSide where
  show (FirstClassSide _ sidesSide aisle window) =
    case sidesSide of
      LeftSide -> show window ++ " " ++ show aisle
      RightSide -> show aisle ++ " " ++ show window

instance Show FirstClassRow where
  show (FirstClassRow rowNum row) = alphaNum (rowNum - 1) ++ "|" ++ showPair "||" row ++ "|"

newtype EconomySection
  = EconomySection [EconomyRow]
  deriving (Eq)

newtype FirstClassSection
  = FirstClassSection [FirstClassRow]
  deriving (Eq)

data Plane = Plane
  { economy :: EconomySection,
    firstClass :: FirstClassSection
  }
  deriving (Eq)

emptyEconomySide :: Int -> SeatSide -> EconomySide
emptyEconomySide row side = EconomySide row side Empty Empty Empty

emptyEconomyRow :: Int -> EconomyRow
emptyEconomyRow row = EconomyRow row (emptyEconomySide row LeftSide, emptyEconomySide row RightSide)

emptyFirstClassSide :: Int -> SeatSide -> FirstClassSide
emptyFirstClassSide row side = FirstClassSide row side Empty Empty

emptyFirstClassRow :: Int -> FirstClassRow
emptyFirstClassRow row = FirstClassRow row (emptyFirstClassSide row LeftSide, emptyFirstClassSide row RightSide)

emptyPlane :: Int -> Int -> Plane
emptyPlane economyRows firstClassRows =
  Plane (EconomySection $ map emptyEconomyRow [1 .. economyRows]) (FirstClassSection $ map emptyFirstClassRow [1 .. firstClassRows])

showFirstClass :: Plane -> String
showFirstClass (Plane _ (FirstClassSection firstClass)) = L.intercalate "\n" $ ["First Class", " |1 2||3 4|"] ++ map show firstClass

showEconomy :: Plane -> String
showEconomy (Plane (EconomySection economy) _) = L.intercalate "\n" $ ["Economy", " |123||456|"] ++ map show economy

instance Show Plane where
  show plane = showFirstClass plane ++ "\n" ++ showEconomy plane

sideMatchesPreferences :: GetSeatable column side => Int -> Set.Set column -> side -> Bool
sideMatchesPreferences numPeople preferences side =
  numFreeSeats side >= numPeople && Set.null (Set.filter (\preference -> getSeat preference side == Full) preferences)

fullSide :: IsSide s => Int -> SeatSide -> s
fullSide = makeSide Full

emptySide :: IsSide s => Int -> SeatSide -> s
emptySide = makeSide Empty

fillSide :: IsSide s => s -> Plane -> Plane
fillSide oldSide = updatePlaneWithNew oldSide (fullSide (sidesRow oldSide) (sidesSide oldSide))

getAllEconomySides :: Plane -> [EconomySide]
getAllEconomySides (Plane (EconomySection econ) _) = do
  EconomyRow _ (left, right) <- econ
  [left, right]

getAllFirstClassSides :: Plane -> [FirstClassSide]
getAllFirstClassSides (Plane _ (FirstClassSection fc)) = do
  FirstClassRow _ (left, right) <- fc
  [left, right]
