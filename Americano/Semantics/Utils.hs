module Americano.Semantics.Utils where

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Americano.Syntax
import Americano.Exceptions

import Americano.Semantics.Domains
import Americano.Semantics.Values

type BinOp a b = a -> a -> b
type BinIntegerOp a = BinOp Integer a
type EnvSelector e = (Env -> e)

checkShellMode :: ERSIO Bool
checkShellMode = ask >>= return . shellMode

-- Environment getters ---------------------------------------------------------

getShellValue :: ERSIO Val
getShellValue = ask >>= return . shellValue

getFun :: Ident -> ERSIO (Maybe Fun)
getFun ident = asks (lookupFEnv ident)

getVarLoc :: Ident -> ERSIO (Maybe Loc)
getVarLoc ident = asks (lookupVEnv ident)

lookupVEnv :: Ident -> Env -> Maybe Loc 
lookupVEnv ident env = M.lookup ident $ vEnv env 

lookupFEnv :: Ident -> Env -> Maybe Fun
lookupFEnv ident env = M.lookup ident $ fEnv env 

-- Store getters ---------------------------------------------------------------

getLocVal :: Loc -> ERSIO Val
getLocVal loc = do
  maybeVal <- gets (lookupStore loc)
  case maybeVal of
    Just val -> return val
    Nothing -> throwError $ RTError $ UndefinedReferenceError loc

lookupStore :: Loc -> Store -> Maybe Val
lookupStore = M.lookup

-- Environment && Store setters ------------------------------------------------

setShellValue :: Val -> ERSIO Env
setShellValue val = asks (\env -> env{shellValue = val})

setFun :: Ident -> Fun -> Env -> ERSIO Env
setFun ident fun env = local (const env) (insertFEnv ident fun)

setNewLoc :: Val -> ERSIO Loc
setNewLoc val = do
  loc <- newloc
  setLocVal loc val
  return loc

setNewVar :: Ident -> Val -> ERSIO Env
setNewVar ident val = do
  loc <- newloc
  setVarLoc ident loc $ setLocVal loc val

setVarLoc :: Ident -> Loc -> ERSIO Env -> ERSIO Env
setVarLoc ident loc = local (insertVEnv ident loc)

setLocVal :: Loc -> Val -> ERSIO Env
setLocVal loc val = modify (insertStore loc val) >> ask

insertVEnv :: Ident -> Loc -> Env -> Env
insertVEnv ident loc env = env {vEnv = M.insert ident loc $ vEnv env}

insertFEnv :: Ident -> Fun -> ERSIO Env
insertFEnv ident fun = do
  env <- ask
  return $ env {fEnv = M.insert ident fun $ fEnv env}

insertStore :: Loc -> Val -> Store -> Store
insertStore = M.insert

-- New store location ----------------------------------------------------------

newloc :: ERSIO Loc
newloc = do
  store <- get
  if (M.null store) then return 0
  else let (i, w) = M.findMax store in return (i + 1)

-- Printing values -------------------------------------------------------------

printShellVal :: Val -> ERSIO ()
printShellVal val = do
  str <- showVal val
  unless (val == VNull) $ liftIO (putStrLn str)
  return ()

printStdoutVal :: Val -> ERSIO ()
printStdoutVal val = case val of
  VNull -> return ()
  VString str -> liftIO (putStrLn str)
  other -> do
    str <- showVal val
    liftIO (putStrLn str)

showVal :: Val -> ERSIO String
showVal val = case val of
  VNull -> return "null"
  VInt i -> return $ show i
  VString str -> return $ concat["\"", str, "\""]
  VBool b -> return $ map C.toLower (show b)
  VArray arr -> do
    let locs = M.elems arr
    elems <- mapM getLocVal locs >>= mapM showVal
    let body = L.intercalate ", " elems 
    return $ "[" ++ body ++ "]"     
  VDict dict -> do
    vals <- mapM getLocVal (M.elems dict) >>= mapM showVal
    keys <- mapM showVal (M.keys dict)
    let keyvals = [concat [k, ": ", v] | (k, v) <- zip keys vals]
    let body = L.intercalate ", " keyvals 
    return $ "({" ++ body ++ "})"
  VRef ref -> getLocVal ref >>= showVal
