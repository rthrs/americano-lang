module Americano.Semantics.Expressions where

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Except

import Americano.Syntax
import Americano.Exceptions

import Americano.Semantics.Domains
import Americano.Semantics.Operators
import Americano.Semantics.Utils
import Americano.Semantics.Values

-- No implicit casting - static type checker should catch all wrong typed
-- expressions... unless it's not implemented :) Yet for cleaner code
-- all pattern match failures are not handled.
denoteExpr :: Expr -> ERSIO Val
denoteExpr x = case x of
  ELVal lval -> getLocVal =<< denoteLVal lval
  
  ECInit cinit -> denoteCInit cinit

  EIncr lval -> addVal lval $ VInt 1

  EDecr lval -> addVal lval $ VInt (-1)

  ELitInt integer -> return $ VInt integer
  ELitTrue -> return $ VBool True
  ELitFalse -> return $ VBool False
  ELitString string -> return $ VString string

  EApp ident exprs -> do
    maybeFun <- getFun ident
    fargs <- mapM denoteExpr exprs
    case maybeFun of
      -- If no return statement occured in
      -- the function body then return VNull.
      Just fun -> (fun fargs >> return VNull)
        `catchError` catchReturnInterruption
      Nothing -> throwError $ RTError $ FunNameError ident

  ENeg expr -> do -- When Functor: denoteExpr expr >>= fmap (*(-1))
    VInt i <- denoteExpr expr
    return $ VInt $ -i 

  ENot expr -> do -- When Functor: denoteExpr expr >>= fmap not
    VBool b <- denoteExpr expr
    return $ VBool $ not b

  EMul expr1 mulop expr2 -> do
    VInt i2 <- denoteExpr expr2
    when (mulop /= OpTimes && i2 == 0) (throwError $ RTError DivByZeroError)
    opf <- denoteMulOp mulop
    VInt i1 <- denoteExpr expr1
    return $ VInt $ opf i1 i2

  EAdd expr1 addop expr2 -> do
    -- Only TInt and TString support.
    opf <- denoteAddOp addop
    vals <- denoteExpr2 expr1 expr2
    case vals of
      (VInt i1, VInt i2) -> return $ VInt $ opf i1 i2
      (VString s1, VString s2) -> do
        when (addop /= OpPlus) (throwError $ RTError UnsupportedOperationError)
        return $ VString $ s1 ++ s2

  ERel expr1 relop expr2 -> do
    -- Only comparision between same types supported.
    val <- compare2Vals relop =<< denoteExpr2 expr1 expr2
    return $ VBool val

  EAnd expr1 expr2 -> do
    -- Only TBool support.
    (VBool val1, VBool val2) <- denoteExpr2 expr1 expr2
    return $ VBool $ val1 && val2

  EOr expr1 expr2 -> do
    -- Only TBool support.
    (VBool val1, VBool val2) <- denoteExpr2 expr1 expr2
    return $ VBool $ val1 && val2

  EAss lval assop expr -> do
    val <- denoteExpr expr
    dval <- getDefaultVal val
    loc <- denoteLVal lval `catchError` catchAssignmentError dval
    denoteAssOp assop loc val

denoteExpr2 :: Expr -> Expr -> ERSIO (Val, Val)
denoteExpr2 expr1 expr2 = do
  val1 <- denoteExpr expr1
  val2 <- denoteExpr expr2
  return (val1, val2)

-- InternalError handlers ------------------------------------------------------

catchReturnInterruption :: InternalError -> ERSIO Val
catchReturnInterruption err = case err of
  Interrupt (Return val) -> return val
  otherwise -> throwError err

catchAssignmentError :: Val -> InternalError -> ERSIO Loc
catchAssignmentError dval err = case err of
  RTError (IndexError len loc) -> do
    -- Negative array indexes are illegal.
    when (len < 0) $ throwError err
    expandArray len loc dval  
  RTError (KeyError key loc _) -> addDictKey key loc
  otherwise -> throwError err

-- Complex types initialization ------------------------------------------------

denoteCInit :: CInit -> ERSIO Val
denoteCInit x = case x of
  InitArrEmp -> do
    loc <- newloc
    setLocVal loc (VArray M.empty)
    return $ VRef loc

  InitArr exprs -> do
    locs <- mapM denoteExpr exprs >>= mapM setNewLoc
    let arr = M.fromList $ zip [0..] locs
    loc <- newloc
    setLocVal loc (VArray arr)
    return $ VRef loc

  InitDict dkeyvals -> do
    pairs <- mapM denoteDKeyVal dkeyvals
    let dict = M.fromList pairs
    loc <- newloc
    setLocVal loc (VDict dict)
    return $ VRef loc

denoteDKeyVal :: DKeyVal -> ERSIO (Val, Loc)
denoteDKeyVal (DictKeyVal expr1 expr2) = do
  val <- denoteExpr expr1
  loc <- setNewLoc =<< denoteExpr expr2
  return (val, loc)

-- Default values depending on rvalues -----------------------------------------

getDefaultVal :: Val -> ERSIO Val
getDefaultVal val = case val of
  VNull -> throwError $ RTError UnsupportedOperationError
  VInt _ -> defaultVInt
  VBool _ -> defaultVBool
  VString _ -> defaultVString
  VArray _ -> defaultVArray
  VDict _ -> defaultVDict
  VRef ref -> getLocVal ref >>= getDefaultVal

defaultVInt :: ERSIO Val
defaultVInt = return $ VInt 0

defaultVBool :: ERSIO Val
defaultVBool = return $ VBool False

defaultVString :: ERSIO Val
defaultVString = return $ VString ""

defaultVArray :: ERSIO Val
defaultVArray = denoteCInit $ InitArr []

defaultVDict :: ERSIO Val
defaultVDict = denoteCInit $ InitDict []

-- Lvalues ---------------------------------------------------------------------

denoteLVal :: LVal -> ERSIO Loc
denoteLVal x = case x of
  LVar ident -> do
    maybeLoc <- getVarLoc ident
    case maybeLoc of
      Just loc -> return loc
      Nothing -> throwError $ RTError $ VarNameError ident

  LArr lval expr -> do
    loc <- denoteLVal lval
    -- Static type checking should prevent passing
    -- here indexes other than integer values.
    VInt i <- denoteExpr expr
    VRef ref <- getLocVal loc
    VArray arr <- getLocVal ref
    case M.lookup i arr of
      Just loc -> return loc
      Nothing -> throwError $ RTError $ IndexError i ref

  LDict lval expr -> do
    loc <- denoteLVal lval
    key <- denoteExpr expr
    VRef ref <- getLocVal loc
    VDict dict <- getLocVal ref
    case M.lookup key dict of 
      Just loc -> return loc
      Nothing -> do
        keyStr <- showVal key
        throwError $ RTError $ KeyError key ref keyStr

-- Values comparision ----------------------------------------------------------

compareArrayVals :: RelOp -> [Val] -> [Val] -> ERSIO Bool
compareArrayVals relop l1 l2 =
  if l1 == [] || l2 == [] then compareEmpty relop l1 l2 
  else do
    let (v1:vs1, v2:vs2) = (l1, l2)
    areEqual <- compare2Vals OpEq (v1, v2)
    if areEqual then compareArrayVals relop vs1 vs2
    else compare2Vals relop (v1, v2)

compareDictVals :: RelOp -> [(Val, Val)] -> [(Val, Val)] -> ERSIO Bool
compareDictVals relop l1 l2 =
  if l1 == [] || l2 == [] then compareEmpty relop l1 l2 
  else do
    let ((k1, v1):kvs1, (k2, v2):kvs2) = (l1, l2)
    areKeysEqual <- compare2Vals OpEq (k1, k2)
    if areKeysEqual then do
      areValuesEqual <- compare2Vals OpEq (v1, v2)
      if areValuesEqual then compareDictVals relop kvs1 kvs2
      else compare2Vals relop (v1, v2)
    else compare2Vals relop (k1, k2)

compareEmpty :: RelOp -> [a] -> [a] -> ERSIO Bool
compareEmpty relop [] [] = return $ not $ any (== relop) [OpLt, OpGt, OpNeq]
compareEmpty relop [] _ = return $ any (== relop) [OpLt, OpLte, OpNeq]
compareEmpty relop _ [] = return $ any (== relop) [OpGt, OpGte, OpNeq]

compare2Vals :: RelOp -> (Val, Val) -> ERSIO Bool
compare2Vals relop vals = case vals of
  (VNull, VNull) -> return False

  (VArray a1, VArray a2) -> do
    vals1 <- getVals (M.elems a1)
    vals2 <- getVals (M.elems a2)
    compareArrayVals relop vals1 vals2
    where getVals = mapM getLocVal

  (VDict d1, VDict d2) -> do
    kvs1 <- getKeyVals d1
    kvs2 <- getKeyVals d2
    compareDictVals relop kvs1 kvs2
    where
      getKeyVals d = mapM coerceMap $ M.toList d
      coerceMap (k, v) = do {vv <- getLocVal v; return (k, vv)}

  (VRef r1, VRef r2) -> do
    val1 <- getLocVal r1
    val2 <- getLocVal r2
    compare2Vals relop (val1, val2)

  -- Other cases, e.g VInt, VString and VBool.
  (val1, val2) -> do
    opf <- denoteRelOp relop
    return $ opf val1 val2

-- Utils -----------------------------------------------------------------------

addVal :: LVal -> Val -> ERSIO Val
addVal lval val = do
  loc <- denoteLVal lval
  lvalval <- getLocVal loc
  denoteAssOp OpAssPlus loc val
  return lvalval -- Return value before assignment.

expandArray :: Integer -> Loc -> Val -> ERSIO Loc
expandArray len loc defaultVal = do
  VArray arr <- getLocVal loc
  let (nlen, olen) = (fromIntegral (len + 1), M.size arr)
  when (nlen <= olen) (throwError $ RTError ExpandArrayError)
  locs <- mapM setNewLoc $ replicate (nlen-olen) defaultVal
  let indexes = map toInteger [olen..nlen-1]
  let filler = M.fromList $ zip indexes locs
  setLocVal loc (VArray $ M.union arr filler)
  return $ last locs

addDictKey :: Val -> Loc -> ERSIO Loc
addDictKey key loc = do
  VDict dict <- getLocVal loc
  vloc <- newloc
  setLocVal loc $ VDict (M.insert key vloc dict)
  return vloc
