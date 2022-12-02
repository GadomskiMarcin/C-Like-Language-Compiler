module QuadGen where

import Prelude as P
import Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import System.IO
import System.FilePath.Posix (takeBaseName, replaceExtension, takeDirectory)
import System.Process (system, callProcess)
import System.Exit        ( exitFailure, exitSuccess )
import Utils
import Data.List as L

import QuadGenStateEnv
import AbsLatte

--genQuadCode :: Program -> Either a b
genQuadCode fp (Prog _ topDefs) = do
   res <- runExceptT $ runStateT (initGenTopDefs topDefs) initialCompState
   case res of
      Left error -> do
        hPutStr stderr "ERROR\n"
        hPutStrLn stderr error
        exitFailure
      Right (_, (CompState _ _ _ _ _ _ _ _ _ code _ _ _ _ _)) -> do
        let finalCode = prologue ++ code
        let file = replaceExtension fp ".ll"
        let bcFile = replaceExtension fp ".bc"
        writeFile file finalCode
        callProcess "llvm-link" [file, "lib/runtime.bc", "-o", bcFile]
        return ()

initGenTopDefs :: [TopDef] -> GenM ()
initGenTopDefs topDefs = do
  fillEnvWithFunctions topDefs
  mapM_ genTopDef topDefs
  genClassMethods
  lsce
  gsce2
  removeUnusedRegs
  printClasses
  printAllBlocks
  printStrings
  return ()

emitArgAsDecl (ident, eval) = do
  emit $ IDeclare ident
  emit $ IAss ident eval

genTopDef (FnDef _ typ ident args bl) = do
  lLabel <- freshLabel
  modify $ \s -> s { currBlock = lLabel}
  let argVars = argsToVars args
  modify $ \s -> s { vars = argVars }
  emit $ IDecFun (latTypeToLLVM typ) ident (unsortedArgs args)
  emit $ IPutLabel lLabel
  mapM_ emitArgAsDecl (Map.toList argVars)
  genBlock bl
  putRetIfVoid typ
  emit ICloseBlock

genTopDef _ = return ()

genClassMethods :: GenM()
genClassMethods = do
  clss <- gets classes
  mapM_ genMethod (Map.toList clss)

genMethod :: (Ident, ClassState) -> GenM()
genMethod (id, (ClassState name vrs funs funsToGen)) = do
  modify $ \s -> s {currClass = (Just name)}
  mapM_ genTopDef (P.map snd (Map.toList funsToGen))

--genBlock :: Block -> GenM ()
genBlock (Block _ stmts) = do
  saveLocals
  vrs1 <- gets vars
  mapM_ genStmt stmts
  vrs2 <- gets vars
  bringBackLocals
  vrs2 <- gets vars
  get

genStmt (Empty _) = do
  get

genStmt (BStmt _ bl) = do
  genBlock bl
  get

genStmt (Decl _ typ itms) = do
  mapM_ (declareItem typ) itms
  get

genStmt (Ass _ e1 expr) = do
  eval <- genExp expr
  vrs <- gets vars
  case e1 of
    (EVar pl ident) -> do
      emit $ IAss ident eval
      modify $ \s -> s { vars = Map.insert ident eval vrs }
    (EAtr pl expr2 id) -> do
      e <- genExp expr2
      case e of
          (CompClass className, reg) -> do
            prop <- getClassVar className id
            case prop of
              Just (t, v) -> do
                if fst eval == t then do
                  r1 <- freshTemp
                  emit $ IGetElemPtrClass r1 (CompClass className, reg) (CompClass className, reg) (CompInt, VInt 0) (CompInt, v)
                  emit $ IStore eval (t, VReg r1)
                else
                  throwError $ "Types do not match " ++ showIdent className ++ showCodePlace pl
              Nothing -> throwError $ "Class " ++ showIdent className ++ " does not contain attribuite " ++ showIdent id ++ showCodePlace pl
          _ -> throwError $ "Class does not contain attribuite " ++ showCodePlace pl
  get

genStmt (Incr _ ident) = do
  a1 <- genExp (EVar Nothing ident)
  a2 <- genBinOp BAdd a1 (CompInt, VInt 1)
  emit $ IAss ident a2
  vrs <- gets vars
  modify $ \s -> s { vars = Map.insert ident a2 vrs }
  get

genStmt (Decr _ ident) = do
  a1 <- genExp (EVar Nothing ident)
  a2 <- genBinOp BSub a1 (CompInt, VInt 1)
  emit $ IAss ident a2
  vrs <- gets vars
  modify $ \s -> s { vars = Map.insert ident a2 vrs }
  get

genStmt (Ret _ expr) = do
  eval <- genExp expr
  emit (IRetVal eval)
  get

genStmt (VRet _) = do
  emit IRetVoid
  get

genStmt (Cond _ epxr stmt_) = do
  eval <- genExp epxr
  let stmt = toBlock stmt_
  case eval of
    (CompBool, VBool True) -> genStmt stmt
    (CompBool, VBool False) -> get
    (CompBool, VReg reg) -> do
      cBlock <- gets currBlock
      stmtBl <- freshLabel
      vrs1 <- gets vars

      modify $ \s -> s {currBlock = stmtBl}
      emit $ IPutLabel stmtBl
      genStmt stmt
      modify $ \s -> s {vars = vrs1}
      afterBl <- freshLabel
      emit $ IBranch afterBl
      afterSt <- gets currBlock

      modify $ \s -> s { currBlock = cBlock }
      emit $ IBranchCond (snd eval) stmtBl afterBl

      let finalT = if (stmtBl == afterSt) then stmtBl else afterSt

      modify $ \s -> s { currBlock = afterBl }
      emit $ IPutLabel afterBl
      --Update blocks ins and outs
      emitPhiBlock finalT cBlock
      getThisCBVars
      get

genStmt (CondElse _ expr stmtT_ stmtF_) = do
  eval <- genExp expr
  let stmtT = toBlock stmtT_
  let stmtF = toBlock stmtF_
  case eval of
    (CompBool, VBool True) -> do
      mapM_ genStmt (fromBlock stmtT)
      get
    (CompBool, VBool False) -> do
      mapM_ genStmt (fromBlock stmtF)
      get
    (CompBool, VReg reg) -> do
      cBlock <- gets currBlock
      trueBl <- freshLabel

      vrs1 <- gets vars

      modify $ \s -> s {currBlock = trueBl}
      emit $ IPutLabel trueBl
      genStmt stmtT
      modify $ \s -> s {vars = vrs1}
      afterT <- gets currBlock
      falseBl <- freshLabel

      modify $ \s -> s {currBlock = cBlock}
      emit $ IBranchCond (snd eval) trueBl falseBl

      modify $ \s -> s {currBlock = falseBl}
      emit $ IPutLabel falseBl
      genStmt stmtF
      modify $ \s -> s {vars = vrs1}
      afterF <- gets currBlock

      afterBl <- freshLabel
--ToDo double check...Looks really heuristic + peephole opt?
      let finalT = if (afterT == trueBl) then trueBl else afterT
      let finalF = if (afterF == falseBl) then falseBl else afterF
      modify $ \s -> s {currBlock = finalF}
      isFinished <- checkIfBothEndInReturn finalT finalF
      if isFinished then do
        get
      else do
        emit $ IBranch afterBl

        modify $ \s -> s {currBlock = finalT}
        emit $ IBranch afterBl

        modify $ \s -> s {currBlock = afterBl}
        emit $ IPutLabel afterBl

        emitPhiBlocks finalT finalF afterBl
        getThisCBVars

        get

genStmt (While _ (ELitTrue _ ) stmtT_) = do
--block for code, block end
  vrs <- gets vars
  cBlock <- gets currBlock
  let stmtT = toBlock stmtT_
  trueBl <- freshLabel
  emit $ IBranch trueBl

  modify $ \s -> s { currBlock = trueBl }
  emit $ IPutLabel trueBl
  --ags <- gets args
  let allIdents = Map.keys vrs --L.\\ (Map.keys ags)
  mapM_ insertNewReg allIdents
  newRegsVars <- gets vars

  genStmt stmtT
  pBDecl <- gets prevBDecl

  pvVrs <- gets prevVars
  afterStmtBl <- gets currBlock
  emit $ IBranch trueBl

  modify $ \s -> s {currBlock = trueBl}
  afterBl <- freshLabel

  reverseInstructions
  popFirstInstr

  let testMap = Map.union pBDecl pvVrs

  mapM_ (emitWhilePhis vrs testMap cBlock afterStmtBl) (toList newRegsVars)
  mapM_ (overrideValInBlock testMap newRegsVars trueBl) (toList vrs)
  emit $ IPutLabel trueBl
  reverseInstructions
  currVars <- gets vars

  modify $ \s -> s { currBlock = afterBl }
  emit $ IPutLabel afterBl
  get


genStmt (While _ (ELitFalse _ ) stmtT_) = do
  get

genStmt (While _ expr stmtT_) = do
  --block for code, block end
  eval <- genExp expr

  vrs <- gets vars
  cBlock <- gets currBlock
  let stmtT = toBlock stmtT_
  condLabel <- freshLabel
  emit $ IBranch condLabel
  trueBl <- freshLabel
  modify $ \s -> s { currBlock = condLabel }
  emit $ IPutLabel condLabel
  --ags <- gets args
  let allIdents = Map.keys vrs --L.\\ (Map.keys ags)
  mapM_ insertNewReg allIdents
  newRegsVars <- gets vars
  (_, r) <- genExp expr

  modify $ \s -> s { currBlock = trueBl }
  emit $ IPutLabel trueBl
  genStmt stmtT
  pBDecl <- gets prevBDecl

  pvVrs <- gets prevVars
  afterStmtBl <- gets currBlock
  emit $ IBranch condLabel
  removeEdge afterStmtBl condLabel

  modify $ \s -> s {currBlock = condLabel}
  afterBl <- freshLabel

  emit $ IBranchCond r trueBl afterBl
  reverseInstructions
  popFirstInstr

  let testMap = Map.union pBDecl pvVrs
  mapM_ (emitWhilePhis vrs testMap cBlock afterStmtBl) (toList newRegsVars)
  mapM_ (\x -> mapM_ (overrideValInBlock testMap newRegsVars x) (toList vrs)) [condLabel..afterStmtBl]
  emit $ IPutLabel condLabel
  reverseInstructions
  currVars <- gets vars
  --modify $ \s -> s { vars = Map.union pBDecl currVars }
  modify $ \s -> s { currBlock = afterBl }
  emit $ IPutLabel afterBl
  get

genStmt (SExp _ expr) = do
  e <- genExp expr
  get

genStmt (For _ _ _ _ _) = do
  get

declareItem :: Type -> Item -> GenM ()
declareItem typ (NoInit _ ident) = do
  vrs <- gets vars
  let llvmTyp = latTypeToLLVM typ
  case llvmTyp of
    CompInt -> do
      emit $ IDeclare ident
      emit $ IAss ident (llvmTyp, VInt 0)
      assBDecl ident
      modify $ \s -> s { vars = Map.insert ident (llvmTyp, VInt 0) vrs }
    CompString -> do
      sMap <- gets stringsMap
      emit $ IDeclare ident
      emit $ IAss ident (CompString, VString (size sMap) 1)
      modify $ \s -> s { stringsMap = Map.insert (Map.size sMap) "" sMap }
      assBDecl ident
      modify $ \s -> s { vars = Map.insert ident (CompString, VString (size sMap) 1) vrs }
    CompBool -> do
      emit $ IDeclare ident
      emit $ IAss ident (llvmTyp, VBool False)
      assBDecl ident
      modify $ \s -> s { vars = Map.insert ident (llvmTyp, VBool False) vrs }
    (CompClass id) -> do
        emit $ IDeclare ident
        emit $ IAss ident ((CompClass id), VNull) --ToDo check if it is necessary
        assBDecl ident
        modify $ \s -> s { vars = Map.insert ident ((CompClass id), VNull) vrs }

declareItem typ (Init _ ident exp) = do
  vrs <- gets vars
  eval <- genExp exp
  emit $ IDeclare ident
  emit $ IAss ident eval
  assBDecl ident
  modify $ \s -> s { vars = Map.insert ident eval vrs}

genExp :: Expr -> GenM Address
genExp (EVar _ i) = getVar i
genExp (ELitInt _ int) = return (CompInt, VInt (fromIntegral int))
genExp (ELitTrue _) = return (CompBool, VBool True)
genExp (ELitFalse _) = return (CompBool, VBool False)
genExp (EString _ str) = do
  sMap <- gets stringsMap
  modify $ \s -> s { stringsMap = Map.insert (Map.size sMap) str sMap }
  return (CompString, VString (size sMap) (length str + 1))

genExp (EApp _ i args) = do
  fcs <- gets funcs
  let (VFun funType _) = fcs ! i
  vs <- mapM (genExp) args
  case funType of
    CompVoid -> do
      emit $ ICall Nothing funType i vs
      return (CompVoid, VUndefined)
    _ -> do
      r <- freshTemp
      emit $ ICall (Just (VReg r)) funType i vs
      return (funType, VReg r)

genExp (Neg _ e) = do
  (typ, val) <- genExp e
  case val of
    (VInt int) -> return (CompInt, VInt (-int))
    (VReg _) -> genBinOp BSub (CompInt, VInt 0) (typ, val)

genExp (Not _ e) = do
  (_, val) <- genExp e
  case val of
    (VBool bool) -> return (CompBool, VBool (not bool))
    (VReg _) -> do
      t <- freshTemp
      emit $ INot t val
      return (CompBool, VReg t)

genExp (EMul pt e1 op e2) = do
  a1 <- genExp e1
  a2 <- genExp e2
  case (a1, a2) of
    ((CompInt, VInt i1), (CompInt, VInt i2)) -> case op of
      Times _ -> return (CompInt, VInt (i1 * i2))
      Div _ -> if i2 == 0 then
       throwError $ showCodePlace pt ++ " div by 0" else
       return (CompInt, VInt (i1 `div` i2))
      Mod _ -> if i2 == 0 then
        throwError $ showCodePlace pt ++ " mod by 0" else
        return (CompInt, VInt (i1 `mod` i2))
    (_, _) -> case op of
      Times _ -> genBinOp BMul a1 a2
      Div _ -> genBinOp BDiv a1 a2
      Mod _ -> genBinOp BMod a1 a2

genExp (EAdd _ e1 op e2) = do
  a1 <- genExp e1
  a2 <- genExp e2
  case (a1, a2) of
    ((CompInt, VInt i1), (CompInt, VInt i2)) -> case op of
      Plus _ -> return (CompInt, VInt (i1 + i2))
      Minus _ -> return (CompInt, VInt (i1 - i2))
    ((CompString, r1), (CompString, r2)) -> do
      t <- freshTemp
      emit $ ICall (Just (VReg t)) CompString (Ident "__concatString__") (a1:[a2])
      return (CompString, VReg t)
    (_, _) -> case op of
      Plus _ -> genBinOp BAdd a1 a2
      Minus _ -> genBinOp BSub a1 a2

genExp (ERel pl e1 op e2) = do
  a1 <- genExp e1
  a2 <- genExp e2
  case (a1, a2) of
    ((CompInt, VInt i1), (CompInt, VInt i2)) -> return (CompBool, VBool (localCmp CompInt op i1 i2))
    ((CompBool, VBool b1), (CompBool, VBool b2)) -> return (CompBool, VBool (localCmp CompBool op b1 b2))
    ((CompInt, i1), (CompInt, i2)) -> do
      t <- freshTemp
      emit $ ICmp t CompInt op a1 a2
      return (CompBool, VReg t)
    ((CompString, a1), (CompString, a2)) -> do
      t <- freshTemp
      emit $ ICmpString t (CompString, a1) (CompString, a2)
      return (CompBool, VReg t)
    ((CompBool, i1), (CompBool, i2)) -> do
      t <- freshTemp
      emit $ ICmp t CompBool op a1 a2
      return (CompBool, VReg t)
    ((CompClass n1, i1), (CompClass n2, i2)) -> do
      if n1 == n2 then do
        t <- freshTemp
        emit $ ICmp t (CompClass n1) op a1 a2
        return (CompBool, VReg t)
      else throwError $ "Classnames do not match:" ++ showIdent n1 ++ " " ++ showIdent n2 ++ showCodePlace pl
    _ -> throwError $ "#TODO - RETEST" ++ showCodePlace pl

genExp (EAnd _ e1 e2) = do
  a1 <- genExp e1
  cBlock <- gets currBlock
  case a1 of
    (CompBool, VBool False) -> return (CompBool, VBool False)
    (CompBool, VBool True) -> genExp e2
    _ -> do
      mid <- freshLabel
      modify $ \s -> s {currBlock = mid}
      emit $ IPutLabel mid
      a2 <- genExp e2
      afterMidL <- gets currBlock
      case a2 of
          (CompBool, VBool False) -> return (CompBool, VBool False)
          (CompBool, VBool True) -> return (CompBool, VBool True)
          _ -> do
            tLabel <- freshLabel
            modify $ \s -> s {currBlock = cBlock}
            emit $ IBranchCond (snd a1) mid tLabel
            modify $ \s -> s {currBlock = afterMidL}
            emit $ IBranch tLabel
            modify $ \s -> s {currBlock = tLabel}
            emit $ IPutLabel tLabel
            t <- freshTemp
            emit $ IPhi t CompBool [(snd a1, cBlock), (snd a2, afterMidL)]
            return (CompBool, VReg t)

genExp (ENull pl e) = do
  case e of
    (EVar _ id) -> do
      cls <- gets classes
      case Map.lookup id cls of
        (Just _) -> return (CompClass id, VNull)
        Nothing -> throwError $ showCodePlace pl ++ " class not declared"
    _ -> throwError $ showCodePlace pl ++ " not a class type"

genExp (ENew _ id) = do
  vrs <- getClassVars id
  t1 <- freshTemp
  t2 <- freshTemp
  t3 <- freshTemp
  t4 <- freshTemp
  emit $ (IGetElemPtr t1 (CompClass id, VNull) (CompClass id, VNull) (CompInt, VInt 1))
  emit $ (IPtrToInt t2 (CompClass id, VReg t1))
  emit $ ICall (Just (VReg t3)) CompString (Ident "__llvmMemcpy__") ([(CompInt, VReg t2)])
  emit $ IBitCast t4 (CompString, VReg t3) (CompClass id)
  return (CompClass id, VReg t4)

genExp (EAtr pl expr id) = do
  e <- genExp expr
  case e of
    (CompClass className, reg) -> do
      prop <- getClassVar className id
      case prop of
        Just (t, v) -> do
          r1 <- freshTemp
          r2 <- freshTemp
          emit $ IGetElemPtrClass r1 e e (CompInt, VInt 0) (CompInt, v)
          emit $ ILoad r2 t (t, VReg r1)
          return (t, VReg r2)
        Nothing -> throwError $ "Class " ++ showIdent className ++ " does not contain attribuite " ++ showIdent id ++ showCodePlace pl
    _ -> throwError $ "Is not a class" ++ show e ++ " " ++ showCodePlace pl

genExp (ECMethodApp _ _ _ _) = do
  return (CompBool, VBool False)

genExp (EArr _ typ expr) = do
  return (CompBool, VBool False)

genExp (EArrGet _ _ _) = do
  return (CompBool, VBool False)

genExp (EOr _ e1 e2) = do
    a1 <- genExp e1
    cBlock <- gets currBlock
    case a1 of
      (CompBool, VBool True) -> return (CompBool, VBool True)
      (CompBool, VBool False) -> genExp e2
      _ -> do
        mid <- freshLabel
        modify $ \s -> s {currBlock = mid}
        emit $ IPutLabel mid
        a2 <- genExp e2
        afterMidL <- gets currBlock
        case a2 of
          (CompBool, VBool True) -> return (CompBool, VBool True)
          (CompBool, VBool False) -> return (CompBool, VBool False)
          _ -> do
            tLabel <- freshLabel
            modify $ \s -> s {currBlock = cBlock}
            emit $ IBranchCond (snd a1) tLabel mid
            modify $ \s -> s {currBlock = afterMidL}
            emit $ IBranch tLabel
            modify $ \s -> s {currBlock = tLabel}
            emit $ IPutLabel tLabel
            t <- freshTemp
            emit $ IPhi t CompBool [(snd a1, cBlock), (snd a2, afterMidL)]
            return (CompBool, VReg t)

genBinOp :: BinOp -> Address -> Address -> GenM Address
genBinOp op (typ, a1) (_, a2) = do
  t <- freshTemp
  emit $ IBinOp t op typ a1 a2
  return (typ, VReg t)
