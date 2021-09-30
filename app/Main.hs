module Main where

import Data.List qualified as L
import Data.List.Split qualified as S
import Data.Set qualified as Set
import Matcher qualified as M
import Plane qualified as P
import System.IO qualified as I
import Text.Printf (printf)
import Text.Read (readMaybe)

initialize :: IO ()
initialize = do
  putStrLn "Welcome to Airplane Manager"
  putStrLn "Type `help` for possible commands"

economyRows :: Int
economyRows = 15

firstClassRows :: Int
firstClassRows = 5

getNumRows :: P.SeatArea -> Int
getNumRows P.FirstClass = firstClassRows
getNumRows P.Economy = economyRows

data ActionUpdate
  = ResetPlane
  | FindEconomySeats Int (Set.Set P.EconomySeatColumn)
  | FindFirstClassSeats Int (Set.Set P.FirstClassSeatColumn)

data ActionRead
  = PrintPlane PlanePart
  | PrintSeatStats

data ActionMeta
  = Help
  | Quit

data Action
  = MetaAction ActionMeta
  | ReadAction ActionRead
  | UpdateAction ActionUpdate

printPrompt :: IO ()
printPrompt = do
  putStr "> "
  I.hFlush I.stdout -- line buffering

commands :: [String]
commands = ["help", "quit", "print", "stats", "reset", "find"]

ambiguityMessage :: [String] -> String
ambiguityMessage options = "That abbreviation was ambiguous. Please specify between:\n  " ++ L.intercalate "\n  " options

economySeatMatcher :: String -> M.MatchResult P.EconomySeatColumn
economySeatMatcher = (P.stringToEconomySeatColumn <$>) . M.matcher "economy seat" P.economySeatColumns

firstClassSeatMatcher :: String -> M.MatchResult P.FirstClassSeatColumn
firstClassSeatMatcher = (P.stringToFirstClassSeatColumn <$>) . M.matcher "first class seat" P.firstClassSeatColumns

sideMatcher :: String -> M.MatchResult P.SeatSide
sideMatcher = (P.stringToSeatSide <$>) . M.matcher "side" P.seatSides

seatAreaMatcher :: String -> M.MatchResult P.SeatArea
seatAreaMatcher = fmap P.stringToSeatArea . M.matcher "seat area" P.seatAreas

data PlanePart
  = PartWholePlane
  | PartArea P.SeatArea
  | PartRow P.SeatArea Int
  | PartSide P.SeatArea Int P.SeatSide
  | PartEconomySeat Int P.SeatSide P.EconomySeatColumn
  | PartFirstClassSeat Int P.SeatSide P.FirstClassSeatColumn

parsePlanePart :: [String] -> Either String PlanePart
parsePlanePart args =
  case args of
    [] -> Right PartWholePlane
    area_ : args' ->
      parsePlanePartArea area_ args'

parsePlanePartArea :: String -> [String] -> Either String PlanePart
parsePlanePartArea area_ args =
  M.userMessage (seatAreaMatcher area_)
    >>= ( \area ->
            let partArea = PartArea area
             in case args of
                  [] -> Right partArea
                  row_ : args' ->
                    parsePlanePartRow area row_ args'
        )

parsePlanePartRow :: P.SeatArea -> String -> [String] -> Either String PlanePart
parsePlanePartRow area row_ args =
  case (readMaybe :: String -> Maybe Int) row_ of
    Just row ->
      let partRow = PartRow area row
       in case args of
            [] -> Right partRow
            side_ : args' ->
              parsePlanePartSide area row side_ args'
    Nothing -> Left "Invalid row"

parsePlanePartSide :: P.SeatArea -> Int -> String -> [String] -> Either String PlanePart
parsePlanePartSide area row side_ args =
  M.userMessage (sideMatcher side_)
    >>= ( \side ->
            let partSide = PartSide area row side
             in case args of
                  [] -> Right partSide
                  seat_ : args' ->
                    ( case area of
                        P.Economy -> parsePlanePartEconomySeat
                        P.FirstClass -> parsePlanePartFirstClassSeat
                    )
                      row
                      side
                      seat_
                      args'
        )

parsePlanePartEconomySeat :: Int -> P.SeatSide -> String -> [String] -> Either String PlanePart
parsePlanePartEconomySeat row side seat_ args =
  case args of
    [] -> M.userMessage (economySeatMatcher seat_) >>= Right . PartEconomySeat row side
    _ -> Left "Too many arguments for plane part"

parsePlanePartFirstClassSeat :: Int -> P.SeatSide -> String -> [String] -> Either String PlanePart
parsePlanePartFirstClassSeat row side seat_ args =
  case args of
    [] -> M.userMessage (firstClassSeatMatcher seat_) >>= Right . PartFirstClassSeat row side
    _ -> Left "Too many arguments for plane part"

validatePlanePart :: PlanePart -> Either String PlanePart
validatePlanePart part =
  let check_ area row =
        if
            | row > getNumRows area -> Just "Row too large"
            | row < 0 -> Just "Row too small"
            | otherwise -> Nothing
      maybeToLeft = maybe (Right part) Left
      check area row = maybeToLeft $ check_ area row
   in case part of
        PartWholePlane -> Right part
        PartArea _ -> Right part
        PartRow area row -> check area row
        PartSide area row _ -> check area row
        PartEconomySeat row _ _ -> check P.Economy row
        PartFirstClassSeat row _ _ -> check P.FirstClass row

cmdToAction :: String -> [String] -> Either String Action
cmdToAction "help" [] = Right $ MetaAction Help
cmdToAction "help" _ = Left "`help` takes no arguments"
cmdToAction "quit" [] = Right $ MetaAction Quit
cmdToAction "quit" _ = Left "`quit` takes no arguments"
cmdToAction "print" args = ReadAction . PrintPlane <$> (parsePlanePart args >>= validatePlanePart)
cmdToAction "stats" [] = Right $ ReadAction PrintSeatStats
cmdToAction "stats" _ = Left "`stats` takes no arguments"
cmdToAction "reset" [] = Right $ UpdateAction ResetPlane
cmdToAction "reset" _ = Left "`reset` takes no arguments"
cmdToAction "find" (area_ : numPeople_ : preferences_) =
  M.userMessage (seatAreaMatcher area_)
    >>= ( \area ->
            case readMaybe numPeople_ of
              Nothing -> Left "The number of people must be a number"
              Just numPeople ->
                if numPeople > P.seatAreaSeats area
                  then Left $ "The side you chose has a maximum of " ++ show (P.seatAreaSeats area) ++ " people per side"
                  else fmap (UpdateAction . FindEconomySeats numPeople . Set.fromList) $ M.userMessage $ M.matchAll economySeatMatcher preferences_
        )
cmdToAction "find" _ = Left "`find` requires a seating area, the number of people, and your seating preferences."
cmdToAction _ _ = error "String couldn't be converted to an Action"

cmdMatcher :: String -> M.MatchResult String
cmdMatcher = M.matcher "commands" commands

parse :: String -> Either String Action
parse cmdline =
  let splitCmdline = S.splitOn " " cmdline
      cmd = head splitCmdline
   in M.userMessage (cmdMatcher cmd) >>= (\cmd -> cmdToAction cmd (tail splitCmdline))

prompt :: P.Plane -> IO Action
prompt plane = do
  printPrompt
  isClosed <- I.isEOF
  if isClosed
    then pure $ MetaAction Quit
    else do
      input <- getLine
      case input of
        "" -> prompt plane
        _ ->
          case parse input of
            Right action -> pure action
            Left str -> do
              putStrLn str
              prompt plane

printHelp :: IO ()
printHelp =
  putStr $
    L.unlines ["Available commands:", "  help", "  quit", "  print [section [row [side seat]]]", "  reset", "  find <section> <numPeople> <seatPreference+>"]

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

chooseEconomySide :: [P.EconomySide] -> IO (Maybe P.EconomySide)
chooseEconomySide possibleSides = do
  putStrLn "Please provide in the format `<rowNum>:<left|right>` or `quit` to cancel."
  putStr ">> "
  I.hFlush I.stdout -- line buffering
  answer' <- getLine
  if answer' `L.isPrefixOf` "quit"
    then pure Nothing -- exit
    else
      let answer = S.splitOn ":" answer'
       in case length answer of
            2 ->
              let maybeRow = readMaybe $ head answer
                  maybeSide = sideMatcher $ answer !! 1
               in case maybeRow of
                    Nothing -> do
                      putStr "The row was not a number. "
                      chooseEconomySide possibleSides
                    Just row ->
                      case maybeSide of
                        M.NoMatch _ _ -> do
                          putStr "The side could not be parsed. "
                          chooseEconomySide possibleSides
                        M.Match side ->
                          -- now we have a row and a side
                          case L.find (\x -> P.sidesRow x == row && P.sidesSide x == side) possibleSides of
                            Nothing -> do
                              putStr "That side is not present in the possibilities. "
                              chooseEconomySide possibleSides
                            Just chosenSide -> pure $ Just chosenSide
                        M.AmbiguousMatch _ _ -> do
                          putStr "The side was ambiguous. "
                          chooseEconomySide possibleSides
            _ -> do
              putStr "Could not be parsed."
              chooseEconomySide possibleSides

chooseFirstClassSide :: [P.FirstClassSide] -> IO (Maybe P.FirstClassSide)
chooseFirstClassSide possibleSides = do
  putStrLn "Please provide in the format `<rowNum>:<left|right>` or `quit` to cancel."
  putStr ">> "
  I.hFlush I.stdout -- override line buffering to print prompt
  answer' <- getLine
  if answer' `L.isPrefixOf` "quit"
    then pure Nothing -- exit
    else
      let answer = S.splitOn ":" answer'
       in case length answer of
            2 ->
              let maybeRow = readMaybe $ head answer
                  maybeSide = sideMatcher $ answer !! 1
               in case maybeRow of
                    Nothing -> do
                      putStr "The row was not a number. "
                      chooseFirstClassSide possibleSides
                    Just row ->
                      case maybeSide of
                        M.NoMatch _ _ -> do
                          putStr "The side could not be parsed. "
                          chooseFirstClassSide possibleSides
                        M.Match side ->
                          -- now we have a row and a side
                          case L.find (\x -> P.sidesRow x == row && P.sidesSide x == side) possibleSides of
                            Nothing -> do
                              putStr "That side is not present in the possibilities. "
                              chooseFirstClassSide possibleSides
                            Just chosenSide -> pure $ Just chosenSide
                        M.AmbiguousMatch _ _ -> do
                          putStr "The side was ambiguous. "
                          chooseFirstClassSide possibleSides
            _ -> do
              putStr "Could not be parsed."
              chooseFirstClassSide possibleSides

reserveSide :: P.IsSide s => s -> Int -> P.Plane -> IO P.Plane
reserveSide chosenSide numPeople plane =
  if P.numFreeSeats chosenSide > numPeople
    then error "Choosing seats in an ambiguous situation is TBA"
    else -- will result in a side that is completely full so we can just return that
      pure $ P.fillSide chosenSide plane

updatePlane :: P.Plane -> ActionUpdate -> IO P.Plane
updatePlane _ ResetPlane = do
  putStrLn "Plane was reset."
  pure $ P.emptyPlane economyRows firstClassRows
updatePlane plane (FindEconomySeats numPeople preferences) =
  let (P.Plane (P.EconomySection econ) _) = plane
      possibleSides = filter (P.sideMatchesPreferences numPeople preferences) $ concatMap (\(P.EconomyRow _ (left, right)) -> [left, right]) econ
   in case length possibleSides of
        0 ->
          ( do
              putStrLn "No matching rows were found."
              pure plane
          )
        1 -> reserveSide (head possibleSides) numPeople plane
        _ -> do
          putStrLn $ "Found multiple rows:" ++ concatMap (\row -> "\n  " ++ P.describe row) possibleSides
          putStr "Pick one. "
          chosenSide <- chooseEconomySide possibleSides
          case chosenSide of
            Nothing -> do
              putStrLn "Okay, cancelling."
              pure plane
            Just chosenSide -> reserveSide chosenSide numPeople plane
updatePlane plane (FindFirstClassSeats numPeople preferences) =
  let (P.Plane _ (P.FirstClassSection fc)) = plane
      possibleSides = filter (P.sideMatchesPreferences numPeople preferences) $ concatMap (\(P.FirstClassRow _ (left, right)) -> [left, right]) fc
   in case length possibleSides of
        0 ->
          ( do
              putStrLn "No matching rows were found."
              pure plane
          )
        1 -> reserveSide (head possibleSides) numPeople plane
        _ -> do
          putStrLn $ "Found multiple rows:" ++ concatMap (\row -> "\n  " ++ P.describe row) possibleSides
          putStr "Pick one. "
          chosenSide <- chooseFirstClassSide possibleSides
          case chosenSide of
            Nothing -> do
              putStrLn "Okay, cancelling."
              pure plane
            Just chosenSide -> reserveSide chosenSide numPeople plane

freeSeatsInRow :: P.TwoSided a b => a -> Int
freeSeatsInRow row =
  P.numFreeSeats (P.getSide P.LeftSide row)
    + P.numFreeSeats (P.getSide P.RightSide row)

printPlaneInfo :: P.Plane -> ActionRead -> IO ()
printPlaneInfo plane (PrintPlane part) =
  let (P.Plane (P.EconomySection econ) (P.FirstClassSection fc)) = plane
   in case part of
        PartWholePlane -> print plane
        PartArea P.Economy -> putStrLn $ P.showEconomy plane
        PartArea P.FirstClass -> putStrLn $ P.showFirstClass plane
        PartRow P.Economy row -> print $ econ !! (row - 1)
        PartRow P.FirstClass row -> print $ fc !! (row - 1)
        PartSide P.Economy row _ -> print $ econ !! (row - 1)
        PartSide P.FirstClass row _ -> print $ fc !! (row - 1)
        PartEconomySeat row side seat ->
          let actualRow = econ !! (row - 1)
              actualSide :: P.EconomySide = P.getSide side actualRow
              actualSeat :: P.SeatState = P.getSeat seat actualSide
           in putStrLn $ P.seatStateHumanReadable actualSeat
        PartFirstClassSeat row side seat ->
          let actualRow = fc !! (row - 1)
              actualSide :: P.FirstClassSide = P.getSide side actualRow
              actualSeat :: P.SeatState = P.getSeat seat actualSide
           in putStrLn $ P.seatStateHumanReadable actualSeat
printPlaneInfo (P.Plane (P.EconomySection econ) (P.FirstClassSection fc)) PrintSeatStats =
  let economyTotal = economyRows * P.seatAreaSeats P.Economy * 2
      fcTotal = firstClassRows * P.seatAreaSeats P.FirstClass * 2
      economyFree = foldr ((+) . freeSeatsInRow) 0 econ
      fcFree = foldr ((+) . freeSeatsInRow) 0 fc
      economyTaken = economyTotal - economyFree
      fcTaken = fcTotal - fcFree
      printStatsRow = printf "Economy: Free %d, Taken %d, Total %d\n"
   in do
        printStatsRow economyFree economyTaken economyTotal
        printStatsRow fcFree fcTaken fcTotal
        printStatsRow (economyFree + fcFree) (economyTaken + fcTaken) (economyTotal + fcTotal)

actionLoop :: P.Plane -> IO ()
actionLoop plane = do
  action <- prompt plane
  case action of
    MetaAction action ->
      case action of
        Quit -> pure () -- exit
        Help -> do
          printHelp
          actionLoop plane
    ReadAction action -> do
      printPlaneInfo plane action
      actionLoop plane
    UpdateAction action -> do
      newPlane <- updatePlane plane action
      actionLoop newPlane

main :: IO ()
main = do
  initialize
  actionLoop $ P.emptyPlane economyRows firstClassRows
