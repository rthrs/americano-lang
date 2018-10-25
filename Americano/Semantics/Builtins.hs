module Americano.Semantics.Builtins where

import Control.Monad.Except

import qualified Data.Map as M

import Text.Read (readMaybe)

import Americano.Syntax
import Americano.Exceptions

import Americano.Semantics.Domains
import Americano.Semantics.Program
import Americano.Semantics.Values

initialVEnv :: VEnv
initialVEnv = M.empty

initialFEnv :: FEnv
initialFEnv = M.fromList
  [ (Ident "toString", toString)
  , (Ident "parseInt", parseInt)
  ]

-- Builtin functions -----------------------------------------------------------

toString :: [FArg] -> ERSIO Env
toString [VInt i] = denoteStmt $ SValReturn $ ELitString $ show i

parseInt :: [FArg] -> ERSIO Env
parseInt [VString s] = case readMaybe s :: Maybe Integer of
  Just i -> denoteStmt $ SValReturn $ ELitInt i
  Nothing -> throwError $ RTError $ ParseIntNoParseError s
