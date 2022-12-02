module TypeCheckerEnvState where

import Data.Map as Map
import Data.List as L

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsLatte

data TCType = Undefined
      | UndeclaredBool
      | UndeclaredInt
      | UndeclaredString
      | StorableBool
      | StorableInt
      | StorableClass Ident
      | StorableString
      | StorableVoid
      | StorableNull
      | StorableFun (TCEnv, TCType, [TCType], [Ident], Block)
      | ErrorType String
      deriving (Show, Eq, Ord, Read)


type Loc = Int
type CodePlace = Maybe (Int, Int)
type TCEnv = Map.Map Ident Loc
type TCState = Map.Map Loc (CodePlace, TCType)
type TCEnvState = ReaderT TCEnv (StateT TCState (ExceptT String IO))

returnValueKey :: Loc
returnValueKey = -1

printIntKey :: Loc
printIntKey = 0

printStringKey :: Loc
printStringKey = 1

errorKey :: Loc
errorKey = 2

readIntKey :: Loc
readIntKey = 3

readStringKey :: Loc
readStringKey = 4

highestPredefinedFunctionKey :: Loc
highestPredefinedFunctionKey = 4

predefinedValues :: [(Loc, (CodePlace, TCType))]
predefinedValues = [
   (returnValueKey, (Nothing, Undefined))
  ,(printIntKey, (Nothing, StorableFun (Map.empty, StorableVoid, [StorableInt], [Ident "x"], Block Nothing [])))
  ,(printStringKey, (Nothing, StorableFun (Map.empty, StorableVoid, [StorableString], [Ident "x"], Block Nothing [])))
  ,(errorKey, (Nothing, StorableFun (Map.empty, StorableVoid, [], [], Block Nothing [])))
  ,(readIntKey, (Nothing, StorableFun (Map.empty, StorableInt, [], [], Block Nothing [])))
  ,(readStringKey, (Nothing, StorableFun (Map.empty, StorableString, [], [], Block Nothing [])))]

initialState :: TCState
initialState = Map.fromList predefinedValues

predefinedFunctionsList :: [(Ident, Loc)]
predefinedFunctionsList = [
   (Ident "__returnValue__", returnValueKey)
  ,(Ident "printInt", printIntKey)
  ,(Ident "printString", printStringKey)
  ,(Ident "error", errorKey)
  ,(Ident "readInt", readIntKey)
  ,(Ident "readString", readStringKey)]

initialEnv :: TCEnv
initialEnv = Map.fromList predefinedFunctionsList

functionsMeetUp :: TCEnvState TCEnv
functionsMeetUp = do
  env <- ask
  store <- get
  local (const env) (setFunctionEnv (toList store))

setFunctionEnv :: [(Loc, (CodePlace, TCType))] -> TCEnvState TCEnv
setFunctionEnv [] = ask
setFunctionEnv ((loc, (place, StorableFun (_, typ, args, idents, bloc))) : fs) = do
  env <- ask
  modify $ Map.insert loc (place, StorableFun (env, typ, args, idents, bloc))
  local (const env) (setFunctionEnv fs)

setFunctionEnv (_:fs) = setFunctionEnv fs

showIdent :: Ident -> String
showIdent (Ident a) = " (Variable: " ++ a ++ ")"

showEVar :: Expr -> String
showEVar (EVar _ i) = showIdent i

showCodePlace :: CodePlace -> String
showCodePlace Nothing = ""
showCodePlace (Just (line, pt)) = "At line " ++ show line ++ ", column " ++ show pt ++ ". "

allTypesMatch t1 t2 = case (t1, t2) of
  (Int _, StorableInt) -> True
  (Str _, StorableString) -> True
  (Bool _, StorableBool) -> True
  (Void _, StorableVoid) -> True
  (Class _ _, StorableClass _) -> True
  (Class _ _, StorableNull) -> True
  (_, StorableNull) -> True
  (_, _) -> False

doesTypesMatch2 t1 t2 = case (t1, t2) of
  (Int _, StorableInt) -> True
  (Int _, UndeclaredInt) -> True
  (Str _, StorableString) -> True
  (Str _, UndeclaredString) -> True
  (Bool _, StorableBool) -> True
  (Bool _, UndeclaredBool) -> True
  (Class _ _, StorableClass _) -> True
  (Class _ _, StorableNull) -> True
  (_, StorableNull) -> True
  (_, _) -> False

doesAssTypesMatch t1 t2 = case (t1, t2) of
  (StorableInt, StorableInt) -> True
  (StorableInt, UndeclaredInt) -> True
  (StorableBool, StorableBool) -> True
  (StorableBool, UndeclaredBool) -> True
  (StorableString, StorableString) -> True
  (StorableString, UndeclaredString) -> True
  (StorableClass i1, StorableClass i2) -> i1 == i2
  (StorableClass i1, _) -> True
  (_, StorableClass i1) -> True
  (_, _) -> False

validateIdent :: Ident -> TCEnvState ()
validateIdent i = do
  env <- ask
  state <- get
  case Map.lookup i env of
    Just loc -> do
      throwError $ "Already declared" ++ showIdent i
    Nothing -> return ()

validateLoopAssign :: Stmt -> String -> TCEnvState ()
validateLoopAssign (Decl pl _ _) loopType =
  throwError $ showCodePlace pl ++ "Cannot declare variables inside the non block: " ++ loopType ++ " statement"
validateLoopAssign _ _ = return ()


newloc :: TCEnvState Loc
newloc = do
  state <- get
  case Map.maxViewWithKey state of
    Just ((k, _), _) -> return (k + 1)
    Nothing -> return 0


getType :: CodePlace -> Ident -> TCEnvState (CodePlace, TCType)
getType pl i = do
  env <- ask
  state <- get
  case Map.lookup i env of
    Just loc ->
      case Map.lookup loc state of
        Just val -> return val
        Nothing -> throwError $ "Variable" ++ showIdent i ++ " is not set"
    Nothing ->
      throwError $ showCodePlace pl ++ "Undeclared variable" ++ showIdent i

getMain :: TCEnvState (CodePlace, TCType)
getMain = do
  env <- ask
  state <- get
  case Map.lookup (Ident "main") env of
    Just loc ->
      case Map.lookup loc state of
        Just val -> return val
        Nothing -> throwError $ "Your program lacks a main function"
    Nothing ->
      throwError $ "Your program lacks a main function"

getLoc :: Ident -> CodePlace -> TCEnvState Loc
getLoc i cp = do
  env <- ask
  case Map.lookup i env of
    Just loc -> return loc
    Nothing -> throwError $ showCodePlace cp ++ "Undeclared var" ++ showIdent i

isReturnOnF :: TCEnvState Bool
isReturnOnF = do
  (_, val) <- getType Nothing (Ident "__returnValue__")
  return (val /= Undefined)

validateBlock :: Block -> TCEnvState ()
validateBlock (Block pl b) = do
  case getDuplicates (validateBlockFoo b []) [] [] of
    [] -> return ()
    l -> throwError $ showCodePlace pl ++ "Following variables are declared more than once: " ++ (asList l)

asList :: [String] -> String
asList ss = (L.intercalate "," ss)

memberL :: (Eq a) => a -> [a] -> Bool
memberL x [] = False
memberL x (y:ys) | x == y = True
                | otherwise = memberL x ys

getDuplicates :: [String] -> [String] -> [String] -> [String]
getDuplicates [] _ non = non
getDuplicates (i:is) prev non =
  case (memberL i prev, memberL i is, memberL i non) of
    (True, _, False) -> getDuplicates is (i:prev) (i:non)
    (_, True, False) -> getDuplicates is (i:prev) (i:non)
    (_, _, _) -> getDuplicates is (i:prev) non

validateBlockFoo :: [Stmt] -> [String] -> [String]
validateBlockFoo [] acc = acc
validateBlockFoo ((Decl _ _ inits):ss) acc = validateBlockFoo ss ((getIdents inits) ++ acc)
validateBlockFoo (_:ss) acc = validateBlockFoo ss acc

getIdents :: [Item] -> [String]
getIdents i = getIdentsFoo i []

getIdentsFoo :: [Item] -> [String] -> [String]
getIdentsFoo [] acc = acc
getIdentsFoo ((NoInit _ (Ident i)):is) acc = getIdentsFoo is (i:acc)
getIdentsFoo ((Init _ (Ident i) _):is) acc = getIdentsFoo is (i:acc)

mapProgTypeToTCType :: Type -> TCType
mapProgTypeToTCType typ = case typ of
  Int _ -> StorableInt
  Str _ -> StorableString
  Bool _ -> StorableBool
  Void _ -> StorableVoid
  Class _ i -> (StorableClass i)
  _ -> StorableNull

argToTCType :: Arg -> TCType
argToTCType (Arg _ typ _) = mapProgTypeToTCType typ

argToIdent :: Arg -> Ident
argToIdent (Arg _ _ i) = i

doesFunArgsMatch :: [TCType] -> [TCType] -> Bool
doesFunArgsMatch [] [] = True
doesFunArgsMatch (x:xs) (y:ys) = if doesAssTypesMatch x y then
   doesFunArgsMatch xs ys
   else
     False

getAllTopDefsIdents :: TCEnvState [Ident]
getAllTopDefsIdents = do
  env <- ask
  return $ Map.keys $ Map.filter ( > highestPredefinedFunctionKey) env


fillFunEnv :: TCEnv -> [TCType] -> [Ident] -> TCEnvState TCEnv
fillFunEnv fEnv [] [] = ask
fillFunEnv fEnv (t:ts) (i:is) = do
  env <- ask
  loc <- newloc
  let newEnv = Map.insert i loc env
  modify $ Map.insert loc (Nothing, t)
  local (const newEnv) (fillFunEnv fEnv ts is)

isTautology (ELitTrue _) = True
isTautology _ = False

isCounterTautology (ELitFalse _) = True
isCounterTautology _ = False
