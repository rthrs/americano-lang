module Main where

import Control.Monad (when)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Americano.Parser
import Americano.Lexer
import Americano.Printer
import Americano.ErrMonad
import Americano.Syntax
import Americano.Interpreter
import Americano.Semantics.Domains

import Debug

type ParseFunc a =  [Token] -> Err a

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage
    [] -> runShell
    fs -> mapM_ runFile fs

runShell :: IO ()
runShell = do
  putStrLn shellHello
  let prompt = if __DEBUG__ then shellDebugPrompt else shellPrompt
  interpretShell prompt initialShellEnv initialStore

runFile :: FilePath -> IO ()
runFile f = interpretFile =<< readFile f

printUsage :: IO ()
printUsage = do
  progName <- getProgName
  putStrLn $ unlines
    [ "Usage: " ++ progName ++ " [OPTION...] [FILE...]"
    , "  --help          Display this help message."
    , "  (no arguments)  Run interactive interpreter."
    , "  (files)         Interpret given files."
    ]
  exitFailure

shellHello = "Welcome to the Americano interpreter!\n"
  ++ "Type 'exit', 'quit' or 'Ctrl-D' to exit."


shellPrompt = "\9749 "

shellDebugPrompt = "(DEBUG MODE) " ++ shellPrompt
