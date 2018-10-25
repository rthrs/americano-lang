module Americano.Semantics.Operators where

import Control.Monad.Except

import Americano.Syntax
import Americano.Exceptions

import Americano.Semantics.Domains
import Americano.Semantics.Values
import Americano.Semantics.Utils

denoteAddOp :: AddOp -> ERSIO (BinIntegerOp Integer)
denoteAddOp x = case x of
  OpPlus -> return (+)
  OpMinus -> return (-)

denoteMulOp :: MulOp -> ERSIO (BinIntegerOp Integer)
denoteMulOp x = case x of
  OpTimes -> return (*)
  OpDiv -> return div
  OpMod -> return mod

denoteRelOp :: (Ord a, Eq a) => RelOp -> ERSIO (BinOp a Bool)
denoteRelOp x = case x of
  OpLt -> return (<)
  OpLte -> return (<=)
  OpGt -> return (>)
  OpGte -> return (>=)
  OpEq -> return (==)
  OpNeq -> return (/=)

denoteAssOp :: AssOp -> Loc -> Val -> ERSIO Val
denoteAssOp x loc val = case x of
  OpAss -> setLocVal loc val >> return val
  OpAssPlus -> execOpAssPlus loc val
  OpAssMinus -> execVIntOpAss loc val (-)
  OpAssTimes -> execVIntOpAss loc val (*)
  OpAssDiv -> do
    when (vInt val == 0) (throwError $ RTError DivByZeroError)
    execVIntOpAss loc val div
  OpAssMod -> do
    when (vInt val == 0) (throwError $ RTError DivByZeroError)
    execVIntOpAss loc val mod

execVIntOpAss :: Loc -> Val -> (BinIntegerOp Integer) -> ERSIO Val
execVIntOpAss loc (VInt rval) opf = do
  VInt lvalval <- getLocVal loc
  let val = VInt $ opf lvalval rval
  setLocVal loc val >> return val

execOpAssPlus :: Loc -> Val -> ERSIO Val
execOpAssPlus loc val = case val of
  VString rval -> do
    VString lvalval <- getLocVal loc
    let cval = VString $ lvalval ++ rval
    setLocVal loc cval >> return cval
  otherwise -> execVIntOpAss loc val (+)
