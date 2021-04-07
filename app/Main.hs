module Main where

import qualified System.IO as I
import qualified Data.List as L
import qualified Data.List.Split as S
import Text.Read(readMaybe)

import qualified Plane as P
import qualified Matcher as M

initialize :: IO ()
initialize = do
  putStrLn "Welcome to Airplane Manager"
  putStrLn "Type `help` for possible commands"

economyRows = 15
firstClassRows = 5
getNumRows :: P.SeatArea -> Int
getNumRows P.FirstClass = firstClassRows
getNumRows P.Economy = economyRows

data ActionUpdate
  = ResetPlane

data ActionRead
  = PrintPlane
  | PrintEconomy
  | PrintEconomyRow Int
  | PrintEconomySeat Int P.SeatSide P.EconomySeatColumn
  | PrintFirstClass
  | PrintFirstClassRow Int
  | PrintFirstClassSeat Int P.SeatSide P.FirstClassSeatColumn
data ActionMeta = Help | Quit
data Action = MetaAction ActionMeta | ReadAction ActionRead | UpdateAction ActionUpdate

printPrompt :: IO ()
printPrompt = do
  putStr "> "
  I.hFlush I.stdout

commands :: [String]
commands =
  [ "help"
  , "quit"
  , "print"
  , "reset"
  ]

ambiguityMessage :: [String] -> String
ambiguityMessage options = "That abbreviation was ambiguous. Please specify between:\n  " ++ L.intercalate "\n  " options

economySeatMatcher :: String -> M.MatchResult
economySeatMatcher = M.matcher P.economySeatColumns
firstClassSeatMatcher :: String -> M.MatchResult
firstClassSeatMatcher = M.matcher P.firstClassSeatColumns

printSeatAction :: P.SeatArea -> Int -> P.SeatSide -> String -> Either Action String
printSeatAction P.Economy row side seat =
  case economySeatMatcher seat of
    M.NoMatch -> Right "Seat must be `aisle`, `middle`, or `window`"
    M.Match seat -> Left $ ReadAction $ PrintEconomySeat row side (P.stringToEconomySeatColumn seat)
    M.AmbiguousMatch options -> Right $ ambiguityMessage options
printSeatAction P.FirstClass row side seat =
  case firstClassSeatMatcher seat of
    M.NoMatch -> Right "Seat must be `aisle` or `window`"
    M.Match seat -> Left $ ReadAction $ PrintFirstClassSeat row side (P.stringToFirstClassSeatColumn seat)

sideMatcher :: String -> M.MatchResult
sideMatcher = M.matcher P.seatSides

printRowAction :: P.SeatArea -> Int -> [String] -> Either Action String
printRowAction P.FirstClass row [] = Left $ ReadAction $ PrintFirstClassRow row
printRowAction P.Economy row [] = Left $ ReadAction $ PrintEconomyRow row
printRowAction area row (side:seat:args) =
  case args of
    [] -> case sideMatcher side of
      M.NoMatch -> Right "Side must be `left` or `right`"
      M.Match side -> printSeatAction area row (P.stringToSeatSide side) seat
      M.AmbiguousMatch sides -> Right $ ambiguityMessage sides
    _ -> Right "Too many arguments to `print`"
printRowAction _ _ _ = -- if side is provided but not seat
  Right "`seat` is required when providing `side`"

printAreaAction :: P.SeatArea -> [String] -> Either Action String
printAreaAction P.Economy [] = Left $ ReadAction PrintEconomy
printAreaAction P.FirstClass [] = Left $ ReadAction PrintFirstClass
printAreaAction area (row:args) =
  case readMaybe row :: Maybe Int of
    Nothing -> Right "The row was not a valid integer"
    Just row -> if
      | row > getNumRows area -> Right $ "The row was above " ++ show (getNumRows area)
      | row < 1 -> Right "The row was not positive"
      | otherwise -> printRowAction area row args

seatAreaMatcher :: String -> M.MatchResult
seatAreaMatcher = M.matcher P.seatAreas

cmdToAction :: String -> [String] -> Either Action String
cmdToAction "help" [] = Left $ MetaAction Help
cmdToAction "help" _ = Right "`help` takes no arguments"
cmdToAction "quit" [] = Left $ MetaAction Quit
cmdToAction "quit" _ = Right "`quit` takes no arguments"
cmdToAction "print" [] = Left $ ReadAction PrintPlane
cmdToAction "print" (area:args) =
  case seatAreaMatcher area of
    M.NoMatch -> Right "Area type must be `economy` or `firstClass`"
    M.Match area -> printAreaAction (P.stringToSeatArea area) args
    -- The following should never happen but is included nonetheless
    M.AmbiguousMatch options -> Right $ ambiguityMessage options

cmdMatcher :: String -> M.MatchResult
cmdMatcher = M.matcher commands
parse :: String -> Either Action String
parse cmdline =
  let splitCmdline = S.splitOn " " cmdline
      cmd = head splitCmdline
  in case cmdMatcher cmd of
    M.NoMatch -> Right "Command not found. Type `help` for possible commands."
    M.Match cmd -> cmdToAction cmd (tail splitCmdline)
    M.AmbiguousMatch cmds -> Right $ "That command was ambiguous. Please specify between:\n  " ++ L.intercalate "\n  " cmds

prompt :: P.Plane -> IO Action
prompt plane = do
  printPrompt
  input <- getLine
  case input of
    "" -> prompt plane
    cmd -> case parse input of
      Left action -> pure action
      Right str -> do
        putStrLn str
        prompt plane

printHelp :: IO ()
printHelp = putStr $ L.unlines
  [ "Available commands:"
  , "  help"
  , "  quit"
  , "  print [section [row [side seat]]]"
  , "  reset"
  ]

updatePlane :: P.Plane -> ActionUpdate -> IO P.Plane
updatePlane plane ResetPlane = do
  putStrLn "Plane was reset."
  pure $ P.emptyPlane 15 5

printPlaneInfo :: P.Plane -> ActionRead -> IO ()
printPlaneInfo plane PrintPlane = print plane
printPlaneInfo plane PrintEconomy = putStrLn $ P.showEconomy plane
printPlaneInfo plane PrintFirstClass = putStrLn $ P.showFirstClass plane
printPlaneInfo (P.Plane (P.EconomySection econ) (P.FirstClassSection fc)) action =
  case action of
    PrintEconomyRow row -> print $ econ !! (row - 1)
    PrintFirstClassRow row -> print $ fc !! (row - 1)
    PrintEconomySeat row side seat ->
      let actualRow = econ !! (row - 1)
          actualSide :: P.EconomySide = P.getSide side actualRow
          actualSeat :: P.SeatState = P.getSeat seat actualSide
      in putStrLn $ case actualSeat of
        P.Empty -> "Empty"
        P.Full -> "Full"
    PrintFirstClassSeat row side seat ->
      let actualRow = fc !! (row - 1)
          actualSide :: P.FirstClassSide = P.getSide side actualRow
          actualSeat :: P.SeatState = P.getSeat seat actualSide
      in putStrLn $ case actualSeat of
        P.Empty -> "Empty"
        P.Full -> "Full"

actionLoop :: P.Plane -> IO ()
actionLoop plane = do
  action <- prompt plane
  case action of
    MetaAction action -> case action of
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
  actionLoop $ P.emptyPlane 15 5
