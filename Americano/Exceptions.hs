module Americano.Exceptions where

import Americano.Syntax

import Americano.Semantics.Values

data InternalError 
  = RTError RuntimeError
  | Interrupt Interruption

instance Show InternalError where
  show (RTError err) = show err
  show (Interrupt err) = show err

data RuntimeError
  = DivByZeroError
  | IndexError Integer Loc
  | KeyError Val Loc String
  | FunNameError Ident
  | VarNameError Ident
  | ParseIntNoParseError String
  | UndefinedReferenceError Loc
  | ExpandArrayError
  | UnsupportedOperationError

instance Show RuntimeError where
  show err = case err of
    DivByZeroError -> "DivByZeroError: division or modulo by zero."
    IndexError i _ -> "IndexError: array index " ++ show i ++ " out of range."
    KeyError val _ key -> "KeyError: " ++ key ++ "."
    FunNameError ident -> "FunNameError: function "
      ++ show ident ++ " is not declared."
    VarNameError ident -> "VarNameError: variable "
      ++ show ident ++ " is not in scope."
    ParseIntNoParseError s -> "ParseIntNoParseError: no parse for string \""
      ++ s ++ "\"."
    UndefinedReferenceError loc -> "[INTERNAL ERROR] "
      ++ "OUPS! :) There's nothing under location: " ++ show loc ++ "."
    ExpandArrayError -> "[INTERNAL ERROR] OUPS! :) ExpandArrayError."
    UnsupportedOperationError -> "UnsupportedOperationError: this runtime " 
      ++ "error was thrown probably due to lack of static type checker "
      ++ "implementation."

data Interruption
  = Break
  | Continue
  | Return Val

instance Show Interruption where
  show err = case err of
    Break -> "SyntaxError: \"break\" outside while loop."
    Continue -> "SyntaxError: \"continue\" outside while loop."
    Return _ -> "SyntaxError: \"return\" outside function."
