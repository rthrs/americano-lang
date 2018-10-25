module Americano.Interpreter where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

import System.IO (stderr, stdout, hPrint, hFlush)
import System.Exit (exitSuccess, exitFailure)
import System.IO.Error

import Americano.ErrMonad
import Americano.Exceptions
import Americano.Parser
import Americano.Syntax

import Americano.Semantics.Builtins
import Americano.Semantics.Domains
import Americano.Semantics.Program
import Americano.Semantics.Values

import Debug

type InterpreterResult = IO (Either InternalError Result) 
type Result = (Env, Store)

runInterpreter :: Program -> Env -> Store -> InterpreterResult 
runInterpreter tree env store = do
  (eitherErr, store) <- runERSIO env store $ denoteProgram tree
  case eitherErr of
    Right env -> return $ Right (env, store)
    Left err -> return $ Left err

interpretShell :: String -> Env -> Store -> IO ()
interpretShell prompt env store = do
  let ienv = env{shellValue = initialShellValue}
  (nenv, nstore) <- interpretLine prompt ienv store
  runDebug $ printResult nenv nstore
  interpretShell prompt nenv nstore

interpretLine :: String -> Env -> Store -> IO Result
interpretLine prompt env store = do
  putStr prompt
  hFlush stdout
  output <- parseLine
  case output of
    Bad info -> putStrLn info >> return (env, store)
    Ok tree -> do
      runDebug $ printTree tree
      eitherRes <- runInterpreter tree env store
      case eitherRes of
        Left err -> hPrint stderr err >> return (env, store)
        Right res -> return res

parseLine :: IO (Err Program)
parseLine = do
  line <- getLine `catchIOError` catchEOF
  when (shouldExit line) exitSuccess
  return $ runParser line

catchEOF :: IOError -> IO a
catchEOF err = if isEOFError err then exitSuccess else ioError err

shouldExit :: String -> Bool
shouldExit str = any (== str) ["exit", "quit"]

interpretFile :: String -> IO ()
interpretFile input = case runParser input of
  Bad info -> putStrLn info >> exitFailure
  Ok tree -> do
    runDebug $ printTree tree
    eitherRes <- runInterpreter tree initialEnv initialStore
    case eitherRes of
      Left err -> hPrint stderr err >> exitFailure
      Right (env, store) -> runDebug $ printResult env store

runParser :: String -> Err Program
runParser input = pProgram $ myLexer input

initialEnv :: Env
initialEnv = Env initialVEnv initialFEnv False initialShellValue

initialShellEnv :: Env
initialShellEnv = initialEnv{shellMode = True}

initialStore :: Store
initialStore = M.empty

initialShellValue :: Val
initialShellValue = VNull
