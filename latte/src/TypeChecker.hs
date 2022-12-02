module TypeChecker where

import Data.Map as Map
import Prelude as P
import Data.Array
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import System.IO
import System.Exit ( exitFailure, exitSuccess )

import TypeCheckerEnvState
import TypeCheckerUtils
import AbsLatte

--typeCheck :: Program -> IO ()
typeCheck (Prog pl topDefs) = runExceptT $ runStateT (runReaderT (storeAndCheckTopDefs (topDefs)) initialEnv) initialState


storeAndCheckTopDefs :: [TopDef] -> TCEnvState ()
storeAndCheckTopDefs [] = typeCheckTopDefs
storeAndCheckTopDefs (topDef:topDefs) = do
  env <- typeCheckTopDef topDef
  local (const env) (storeAndCheckTopDefs topDefs)


typeCheckTopDef :: TopDef -> TCEnvState TCEnv
typeCheckTopDef (FnDef pt varType ident args block) = do
  env <- ask
  state <- get
  loc <- newloc
  validateIdent ident
  if areArgsUnique args
    then
      if validArgsTypes args
        then do
          let newEnv = Map.insert ident loc env
          let parsedArgs = P.map argToTCType args
          let idents = P.map argToIdent args
          modify $ Map.insert loc (pt, StorableFun (newEnv, mapProgTypeToTCType varType, parsedArgs, idents, block))
          return newEnv
      else
        throwError $ showCodePlace pt ++ "Argument of type void in function" ++ showIdent ident
    else
      throwError $ showCodePlace pt ++ "Argument repetitions in function" ++ showIdent ident

typeCheckTopDef _ = do
  ask

typeCheckTopDefs :: TCEnvState ()
typeCheckTopDefs = do
  env <- ask
  (_ , StorableFun (_, retType, _, _, block)) <- getMain
  functionsMeetUp
  allTopDefsIdents <- getAllTopDefsIdents
  checkAllFuncs allTopDefsIdents
  return ()

checkAllFuncs [] = return ()
checkAllFuncs (f:fs) = do
  env <- ask
  local (const env) (typeCheckFunction f)
  local (const env) (checkAllFuncs fs)
  return ()

typeCheckFunction :: Ident -> TCEnvState ()
typeCheckFunction ident = do
  (pt , StorableFun (fEnv, retType, types, idents, bloc)) <- getType Nothing ident
  modify $ Map.insert returnValueKey (pt, Undefined)
  newFEnv <- local (const fEnv) (fillFunEnv fEnv types idents)
  local (const newFEnv) (interprateBlockStart bloc)
  (_, accualRet) <- getType Nothing (Ident "__returnValue__")
  if retType == accualRet || (accualRet == Undefined && retType == StorableVoid) || accualRet == StorableNull
    then return ()
    else case accualRet of
      Undefined -> throwError $ showCodePlace pt ++ "Function did not return a value" ++ showIdent ident
      _ -> throwError $ showCodePlace pt ++ "Function returned wrong type value" ++ showIdent ident

interprateBlockStart :: Block -> TCEnvState TCEnv
interprateBlockStart (Block pt b) = do
  validateBlock (Block pt b)
  validateBlockLineByLine (Block pt b)
  ask

validateBlockLineByLine :: Block -> TCEnvState TCEnv
validateBlockLineByLine (Block _ []) = do
  ask

validateBlockLineByLine (Block pt (s:ss)) = do
  env <- validateStmt s
  isReturnOn <- isReturnOnF
  if isReturnOn
    then return env
    else local (const env) (validateBlockLineByLine (Block pt ss))


validateStmt :: Stmt -> TCEnvState TCEnv
validateStmt (Empty a) = do
  ask

validateStmt (BStmt _ block) = do
  interprateBlockStart block
  ask

validateStmt (Decl pt typ items) = declare pt typ items

validateStmt (Ass pl e1 expr) = do
    env <- ask
    state <- get
    case e1 of
      (EVar pl2 ident) -> do
        (_, currType) <- getType pl ident
        eType <- evalExpType expr
        if doesAssTypesMatch eType currType
          then do
            currLoc <- getLoc ident pl
            modify $ Map.insert currLoc (pl, eType)
            ask
          else
            throwError $ showCodePlace pl ++ "Assign types do not match " ++ showIdent ident
      (EAtr _ e2 i3) -> do
        ask
      _ -> throwError $ showCodePlace pl ++ "Assign types do not match "

validateStmt (Incr pl ident) = do
  (_, val) <- getType pl ident
  case val of
    StorableInt -> ask
    UndeclaredInt -> throwError $ showCodePlace pl ++ "Undeclared value" ++ showIdent ident
    _ -> throwError $ showCodePlace pl ++ "Wrong increment type " ++ showIdent ident

validateStmt (Decr pl ident) = do
  (_, val) <- getType pl ident
  case val of
    StorableInt -> ask
    UndeclaredInt -> throwError $ showCodePlace pl ++ "Undeclared value" ++ showIdent ident
    _ -> throwError $ showCodePlace pl ++ "Wrong decrement type " ++ showIdent ident

validateStmt (Ret pl expr) = do
  eval <- evalExpType expr
  modify $ Map.insert returnValueKey (pl, eval)
  ask

validateStmt (VRet pl) = do
  modify $ Map.insert returnValueKey (pl, StorableVoid)
  ask

validateStmt (Cond pl expr stmt) = do
  env <- ask
  store <- get
  expType <- evalExpType expr
  case expType of
    StorableBool -> do
      if isTautology expr then do
        validateStmt stmt
      else if isCounterTautology expr then
        return env
        else do
          local (const env) (validateStmt stmt)
          modify $ const store
          return env
    _ -> throwError $ showCodePlace pl ++ "condition type is not a boolean"

validateStmt (CondElse pl expr stmt1 stmt2) = do
  expType <- evalExpType expr
  env <- ask
  store <- get
  case expType of
    StorableBool -> do
      if isTautology expr then
        validateStmt stmt1
      else if isCounterTautology expr then
        validateStmt stmt2
        else do
          local (const env) (validateStmt stmt1)
          (_, retType1) <- getType Nothing (Ident "__returnValue__")
          local (const env) (validateStmt stmt2)
          (_, retType2) <- getType Nothing (Ident "__returnValue__")
          modify $ const store
          if retType1 == retType2 then do
            modify $ Map.insert returnValueKey (pl, retType1)
            return env
          else
            return env
    _ -> throwError $ showCodePlace pl ++ "condition type is not a boolean"

validateStmt (While pl expr stmt) = do
  expType <- evalExpType expr
  case expType of
    StorableBool -> do
       validateStmt stmt
       ask
    _ -> throwError $ showCodePlace pl ++ "condition type is not a boolean"

validateStmt (SExp pl expr) = do
  evalExpType expr
  ask

--declare :: CodePlace -> Type -> [Item] -> TCEnvState TCEnv
declare _ _ [] = ask
declare pl sType (NoInit pl2 i : is) = do
  env <- ask
  state <- get
  loc <- newloc
  case sType of
    Int _ -> modify $ Map.insert loc (pl2, StorableInt)
    Str _ -> modify $ Map.insert loc (pl2, StorableString)
    Bool _ -> modify $ Map.insert loc (pl2, StorableBool)
    Class _ i -> modify $ Map.insert loc (pl2, StorableClass i)
    Void _ -> throwError $ showCodePlace pl2 ++ "Can't declare void as type" ++ showIdent i

  local (const $ Map.insert i loc env) (declare pl sType is)

declare pl sType (Init pl2 i e : is) = do
  env <- ask
  state <- get
  loc <- newloc
  evalved <- evalExpType e
  if doesTypesMatch2 sType evalved
    then modify $ Map.insert loc (pl2, evalved)
    else throwError $ showCodePlace pl2 ++ "Types do not match" ++ showIdent i
  local (const $ Map.insert i loc env) (declare pl sType is)

evalExpType :: Expr -> TCEnvState TCType
evalExpType (EVar pl ident) = do
  (_, typ) <- getType pl ident
  return typ

evalExpType (ENull pl expr) = do
  case expr of
    EVar _ id -> return $ StorableClass id
    _ -> throwError $ showCodePlace pl ++ "Null operator on non-class variable"


evalExpType (ENew _ i) = do
  return $ StorableClass i

evalExpType (EAtr _ _ _) = do
  return $ StorableNull

evalExpType (ELitInt _ _) = do
  return StorableInt

evalExpType (ELitTrue _) = do
  return StorableBool

evalExpType (ELitFalse _) = do
  return StorableBool

evalExpType (EApp pl ident exprs) = do
  (_, StorableFun (_, fType, args, idents, _)) <- getType pl ident
  evalved <- mapM evalExpType exprs
  if length evalved == length args
    then do
      if doesFunArgsMatch args evalved then
        return fType
      else
        throwError $ showCodePlace pl ++ "Arguments types are not proper in function " ++ showIdent ident
    else throwError $ showCodePlace pl ++ "Missing arguments inside the function call" ++ showIdent ident

evalExpType (EString _ _) = do
  return StorableString

evalExpType (Neg pl exp) = do
  expType <- evalExpType exp
  case expType of
    StorableInt -> return StorableInt
    _ -> throwError $ showCodePlace pl ++ "Neg expects integer type"

evalExpType (Not pl exp) = do
  expType <- evalExpType exp
  case expType of
    StorableBool -> return StorableBool
    _ -> throwError $ showCodePlace pl ++ "Not expects boolean type"

evalExpType (EMul pl e1 op e2) = do
  evalved1 <- evalExpType e1
  evalved2 <- evalExpType e2
  case (evalved1, evalved2) of
    (StorableInt, StorableInt) -> return StorableInt
    (_, _) -> throwError $ showCodePlace pl ++ "Mulitplication operator require expresions to be integer type"

evalExpType (EAdd pl e1 op e2) = do
  evalved1 <- evalExpType e1
  evalved2 <- evalExpType e2
  case op of
    Plus p1 -> case (evalved1, evalved2) of
      (StorableInt, StorableInt) -> return StorableInt
      (StorableString, StorableString) -> return StorableString
      (StorableNull, StorableNull) -> return StorableInt
      (_, _) -> throwError $ showCodePlace p1 ++ "Plus operator require expresions to be the same type"
    Minus p2 -> case (evalved1, evalved2) of
      (StorableNull, StorableNull) -> return StorableNull
      (StorableInt, StorableInt) -> return StorableInt
      (_, _) -> throwError $ showCodePlace p2 ++ "Minus operator require expresions to be the same type"

evalExpType (ERel pl e1 op e2) = do
  evalved1 <- evalExpType e1
  evalved2 <- evalExpType e2
  case (evalved1, evalved2) of
    (StorableBool, StorableBool) -> return StorableBool
    (StorableInt, StorableInt) -> return StorableBool
    (StorableString, StorableString) -> return StorableBool
    (StorableClass i, StorableClass i2) -> if i == i2 then return $ StorableBool
      else throwError $ showCodePlace pl ++ "Relation operator require expresions to be the same type"
    (StorableNull, a) -> return a
    (a, StorableNull) -> return a
    (_, _) -> throwError $ showCodePlace pl ++ "Relation operator require expresions to be the same type"

evalExpType (EAnd pl e1 e2) = do
  evalved1 <- evalExpType e1
  evalved2 <- evalExpType e2
  case (evalved1, evalved2) of
    (StorableBool, StorableBool) -> return StorableBool
    (_, _) -> throwError $ showCodePlace pl ++ "AND expects boolean type"

evalExpType (EOr pl e1 e2) = do
  evalved1 <- evalExpType e1
  evalved2 <- evalExpType e2
  case (evalved1, evalved2) of
    (StorableBool, StorableBool) -> return StorableBool
    (_, _) -> throwError $ showCodePlace pl ++ "OR expects boolean type"
