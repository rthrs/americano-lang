module Americano.Semantics.Program where
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except

import Americano.Syntax
import Americano.Exceptions

import Americano.Semantics.Domains
import Americano.Semantics.Expressions
import Americano.Semantics.Utils
import Americano.Semantics.Values

import Debug

--Programs ---------------------------------------------------------------------

denoteProgram :: Program -> ERSIO Env
denoteProgram (Prog phrases) = do
  env <- evalPhrases phrases
  let val = shellValue env
  -- Print last computed expression value in shell mode.
  checkShellMode >>= flip when (printShellVal val)
  return env

evalPhrases :: [Phrase] -> ERSIO Env
evalPhrases [] = ask
evalPhrases (p:ps) = do
  env <- denotePhrase p
  local (const env) (evalPhrases ps)

denotePhrase :: Phrase -> ERSIO Env
denotePhrase x = case x of
  PDecl decl -> denoteDecl decl
  PStmt stmt -> denoteStmt stmt
  PExpr expr -> do
    val <- denoteExpr expr
    setShellValue val
  PBlock block -> denoteBlock block

-- Ignore environment from different block, but save it's
-- last computation value for shell mode purposes.
denoteBlock :: Block -> ERSIO Env
denoteBlock (Block phrases) = do
  penv <- evalPhrases phrases
  val <- local (const penv) getShellValue
  setShellValue val

-- Declarations ----------------------------------------------------------------

denoteDecl :: Decl -> ERSIO Env
denoteDecl x = case x of
  DVar typeb items -> declareItems typeb items
  DFun typeb ident dargs block -> declareFun typeb ident dargs block 

declareItems :: TypeB -> [Item] -> ERSIO Env 
declareItems _ [] = ask
declareItems typeb (i:is) = do
  env <- denoteItem typeb i
  local (const env) (declareItems typeb is)

denoteItem :: TypeB -> Item -> ERSIO Env
denoteItem typeb x = case x of
  -- Static anylysys should prevent function and variable
  -- redefinitions in the current scope, which can happen here.
  DNoInit ident -> setNewVar ident =<< getDefaultTVal typeb
  DInit ident expr -> setNewVar ident =<< denoteExpr expr

declareFun :: TypeB -> Ident -> [DArg] -> Block -> ERSIO Env
declareFun rettype ident dargs block = do
  env <- ask
  setFun ident (makeFun ident dargs block env) env

makeFun :: Ident -> [DArg] -> Block -> Env -> Fun
makeFun ident dargs block env fargs = do
  -- Provide recursive functions and static binding.
  nenv1 <- setFun ident (makeFun ident dargs block env) env
  nenv2 <- local (const nenv1) (setDArgs dargs fargs)
  local (const nenv2) (denoteBlock block)

setDArgs :: [DArg] -> [FArg] -> ERSIO Env
setDArgs [] [] = ask
setDArgs (da:das) (fa:fas) = do
  env <- denoteDArg da fa
  local (const env) (setDArgs das fas)

denoteDArg :: DArg -> FArg -> ERSIO Env
denoteDArg (DArg typeb ident) = setNewVar ident

--Statements -------------------------------------------------------------------

denoteStmt :: Stmt -> ERSIO Env
denoteStmt x = case x of
  SEmpty -> ask

  SBreak -> throwError $ Interrupt Break
  
  SContinue -> throwError $ Interrupt Continue

  SReturn -> throwError $ Interrupt $ Return VNull

  SValReturn expr -> do
    val <- denoteExpr expr
    throwError $ Interrupt $ Return val

  SCond expr phrase -> do
    VBool cond <- denoteExpr expr
    if cond then denotePhrase phrase else ask

  SCondElse expr phrase1 phrase2 -> do
    VBool cond <- denoteExpr expr
    if cond then denotePhrase phrase1 else denotePhrase phrase2

  SWhile expr phrase -> do
    VBool cond <- denoteExpr expr
    if cond then
      (denotePhrase phrase >> denoteStmt x)
        `catchError` catchLoopInterruption x
    else ask

  SPrint expr -> denoteExpr expr >>= printStdoutVal >> ask

catchLoopInterruption :: Stmt -> InternalError -> ERSIO Env
catchLoopInterruption x err = case err of
  Interrupt Break -> ask 
  Interrupt Continue -> denoteStmt x
  otherwise -> throwError err

-- Default values depending on type --------------------------------------------

getDefaultTVal :: TypeB -> ERSIO Val
getDefaultTVal (TPrimitive t) = getDefaultPrimitiveVal t
getDefaultTVal (TComplex t) = getDefaultComplexVal t

getDefaultPrimitiveVal :: TypeP -> ERSIO Val
getDefaultPrimitiveVal t = case t of
  TInt -> defaultVInt
  TBool -> defaultVBool
  TString -> defaultVString

getDefaultComplexVal :: TypeC -> ERSIO Val
getDefaultComplexVal t = case t of
  TArray _ -> defaultVArray
  TDict _ _-> defaultVDict
