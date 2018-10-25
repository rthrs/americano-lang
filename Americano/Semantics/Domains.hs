{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Americano.Semantics.Domains where

import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Fail

import Americano.Syntax
import Americano.Exceptions

import Americano.Semantics.Values

type ERSIO a = ExceptT InternalError (ReaderT Env (StateT Store IO)) a

data Env = Env
  { vEnv :: VEnv
  , fEnv :: FEnv
  , shellMode :: Bool
  , shellValue :: Val
  } deriving (Show)

type Store = M.Map Loc Val

type VEnv = M.Map VName Loc
type VName = Ident

type FEnv = M.Map FName Fun
type Fun = [FArg] -> ERSIO Env
type FName = Ident
type FArg = Val

instance Show Fun where
  show _ = "<<function>>"

runERSIO :: Env -> Store -> ERSIO Env -> IO (Either InternalError Env, Store)
runERSIO env store = flip runStateT store . flip runReaderT env . runExceptT
