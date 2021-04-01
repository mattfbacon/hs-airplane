module Main where

import qualified System.IO as I
import qualified Data.List as L
import qualified Data.List.Split as S
import Text.Read(readPrec)

import qualified Plane as P

initialize :: IO ()
initialize = do
  putStrLn "Welcome to Airplane Manager"
  putStrLn "Type `help` for possible commands"

data ActionUpdate = SomeChange
data ActionRead = PrintPlane
data ActionMeta = Help | Quit
data Action = MetaAction ActionMeta | ReadAction ActionRead | UpdateAction ActionUpdate

printPrompt :: IO ()
printPrompt = do
  putStr "> "
  I.hFlush I.stdout

commands =
  [ "help"
  , "quit"
  , "print"
  ]

cmdToAction :: String -> Action
cmdToAction "help" = MetaAction Help
cmdToAction "quit" = MetaAction Quit
cmdToAction "print" = ReadAction PrintPlane

parse :: String -> Either Action String
parse cmdline =
  let cmd = head (S.splitOn " " cmdline)
      possibilities = filter (cmd `L.isPrefixOf`) commands
  in case length possibilities of
    0 -> Right "Command not found. Type `help` for possible commands."
    1 -> Left $ cmdToAction $ head possibilities
    _ -> Right $ "That command was ambiguous. Please specify between:\n  " ++ L.intercalate "\n  " possibilities

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
printHelp = putStrLn $ "Available commands:\n  " ++ L.intercalate "\n  " commands

updatePlane :: P.Plane -> ActionUpdate -> IO P.Plane
updatePlane plane SomeChange = do
  putStrLn "pong"
  pure plane -- unmodified

printPlaneInfo :: P.Plane -> ActionRead -> IO ()
printPlaneInfo plane PrintPlane = print plane

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
  actionLoop P.emptyPlane
