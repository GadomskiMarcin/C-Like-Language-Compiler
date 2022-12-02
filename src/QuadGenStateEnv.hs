module QuadGenStateEnv where

import Data.Map as Map
import Data.Char (toLower)
import Prelude as P
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List as L
import Data.Function (on)
import System.IO
import Utils

import AbsLatte

data CompType = CompInt
 | CompBool
 | CompString
 | CompVoid
 | CompClass Ident
 | CompArr Val CompType
 deriving (Show, Eq, Ord)
 
showCT :: CompType -> String
showCT CompInt = "i32"
showCT CompBool = "i1"
showCT CompString = "i8*"
showCT CompVoid = "void"
showCT (CompClass (Ident id)) = "%class." ++ id ++ "*"

data Val = VReg String
  | VInt Int
  | VString Int Int
  | VBool Bool
  | VFun CompType [CompType]
  | VUndefined
  | VNull
  | VPointer Val
  deriving (Show, Eq, Ord)

showVal :: Val -> String
showVal (VReg reg) = reg
showVal (VInt int) = show int
showVal (VString str size) = "getelementptr inbounds ([" ++ show size ++ " x i8], [" ++ show size ++ " x i8]* @__string__" ++ show str ++ ", i64 0, i64 0)"
showVal (VBool bool) = P.map toLower (show bool)
showVal (VFun _ _) = ""
showVal VUndefined = ""
showVal (VPointer val) = showVal val ++ "*"
showVal VNull = "null"

data BinOp = BAdd
  | BSub
  | BDiv
  | BMul
  | BMod
  deriving (Show, Ord, Eq)

showOp :: BinOp -> String
showOp BAdd = "add"
showOp BSub = "sub"
showOp BDiv = "sdiv"
showOp BMul = "mul"
showOp BMod = "srem"

type Address = (CompType, Val)

showRelOp :: RelOp -> String
showRelOp (LTH _) = "slt"
showRelOp (LE _) = "sle"
showRelOp (GTH _) = "sgt"
showRelOp (GE _) = "sge"
showRelOp (EQU _) = "eq"
showRelOp (NE _) = "ne"

data Instr
  = IRetVoid
  | IRetVal Address
  | ICall (Maybe Val) CompType Ident [Address]
  | IStore Address Address
  | ILoad String CompType Address
  | IBinOp String BinOp CompType Val Val
  | IBranch Int
  | IBranchCond Val Int Int
  | IPhi String CompType [(Val, Int)]
  | IPutLabel Int
  | ICloseBlock
  | IDeclare Ident
  | IStartBlock
  | INot String Val
  | IDecFun CompType Ident [Address]
  | ICmp String CompType RelOp Address Address
  | ICmpString String Address Address
  | IAss Ident Address
  | IGetElemPtr String Address Address Address
  | IGetElemPtrClass String Address Address Address Address
  | IPtrToInt String Address
  | IBitCast String Address CompType

  deriving (Show, Eq, Ord)

showAddress :: Address -> String
showAddress (ct, val) = showCT ct ++ " " ++ showVal val

iPro :: String
iPro = "  "

showPoss :: [(Val, Int)] -> String
showPoss ((val, int): []) = "[ " ++ showVal val ++ ", " ++ "%L" ++ show int ++ " ]"
showPoss ((val, int): xs) = "[ " ++ showVal val ++ ", " ++ "%L" ++ show int ++ " ], " ++ showPoss xs

showInstr :: Instr -> String
showInstr (IRetVoid) = iPro ++ "ret void"
showInstr (IRetVal addr) = iPro ++ "ret " ++ showAddress addr
showInstr (ICall maybeReg cTyp (Ident ident) vals) = case maybeReg of
   Nothing -> iPro ++ "call void @" ++ ident ++ "(" ++ intercalate ", " (P.map showAddress vals) ++ ")"
   Just (VReg reg) -> iPro ++ reg ++ " = call " ++ showCT cTyp ++ " @" ++ ident ++ "(" ++ intercalate ", " (P.map showAddress vals) ++ ")"
showInstr (IBinOp reg op typ a1 a2) = iPro ++ reg ++ " = " ++ showOp op ++ " " ++ showCT typ ++ " " ++ showVal a1 ++ ", " ++ showVal a2
showInstr (IBranch int) = iPro ++ "br label %L" ++ show int
showInstr (IPhi reg typ poss) = iPro ++ reg ++ " = phi " ++ showCT typ ++ " " ++ showPoss poss
showInstr (IBranchCond v l1 l2) = iPro ++ "br i1 " ++ showVal v ++ ", label %L" ++ show l1 ++ ", label %L" ++ show l2
showInstr (ICloseBlock) = "}\n"
showInstr (INot t val) = iPro ++ t ++ " = xor i1 1, " ++ showVal val
showInstr (IDecFun cType (Ident id) cts) = "define " ++ showCT cType ++ " @" ++ id ++ "(" ++ intercalate ", " (P.map (showAddress) cts) ++ ")" ++ " {"
showInstr (ICmp reg typ op a1 a2) = iPro ++ reg ++ " = " ++ "icmp " ++ showRelOp op ++ " " ++ showCT typ ++ " " ++ showVal (snd a1) ++ ", " ++ showVal (snd a2)
showInstr (ICmpString reg a1 a2) = iPro ++ reg ++ " = call i1 @__cmpString__(" ++ showAddress a1 ++ ", " ++ showAddress a2 ++ ")"
showInstr (IPutLabel int) = "L" ++ show int ++ ":"
showInstr (IAss (Ident a) address) = iPro ++ "(" ++ show a ++ " = " ++ showAddress address ++ ")"
showInstr (IGetElemPtr reg (t1, _) a2 a3) = iPro ++ reg ++ " = getelementptr " ++ (dropPointer (showCT t1)) ++ ", " ++ showAddress a2 ++ ", " ++ showAddress a3
showInstr (IGetElemPtrClass reg (t1, _) a2 a3 a4) = iPro ++ reg ++ " = getelementptr " ++ (dropPointer (showCT t1)) ++ ", " ++ showAddress a2 ++ ", " ++ showAddress a3 ++ ", " ++ showAddress a4
showInstr (IPtrToInt reg a) = iPro ++ reg ++ " = ptrtoint " ++ showAddress a ++ " to i32"
showInstr (IBitCast reg a1 typ) = iPro ++ reg ++ " = bitcast " ++ showAddress a1 ++ " to " ++ showCT typ
showInstr (IStore a1 (t2, v2)) = iPro ++ "store " ++ showAddress a1 ++ ", " ++ showCT t2 ++ "* " ++ showVal v2 ++ ", align 8"
showInstr (ILoad reg cT (t2, v2)) = iPro ++ reg ++ " = load " ++ showCT cT ++ ", " ++ showCT t2 ++ "* " ++ showVal v2 ++ " , align 8"

showInstr (IStartBlock) = ""
showInstr (IDeclare _) = iPro ++ ""

type Instrs = [Instr]

data CodeBlock = CodeBlock { instructions :: Instrs
                           , label :: Int
                           , coming :: [Int]
                           , terminating :: [Int]
}

data ClassState = ClassState {
    name :: Ident
  , classVars :: Map.Map Ident Address
  , classFuncs :: Map.Map Ident Val
  , funcsBlock :: Map.Map Ident TopDef
}

data CompState = CompState {
    currBlock :: Int
  , blocks :: Map.Map Int CodeBlock
  , vars :: Map.Map Ident Address
  , funcs :: Map.Map Ident Val
  , lastReg :: Int
  , lastLabel :: Int
  , stringsMap :: Map.Map Int String
  , localsSave :: Map.Map Ident Address
  , prevVars :: Map.Map Ident Address
  , code :: String
  , classes :: Map.Map Ident ClassState
  , bDecl :: Map.Map Ident Address
  , prevBDecl :: Map.Map Ident Address
  , edges :: Map.Map Int [Int]
  , currClass :: Maybe Ident
}

predefinedFunctions = Map.fromList [ (Ident "printInt", (VFun CompVoid [CompInt]))
  , (Ident "printString", (VFun CompVoid [CompString]))
  , (Ident "error", (VFun CompVoid []))
  , (Ident "readInt", (VFun CompInt []))
  , (Ident "readString", (VFun CompString []))
  , (Ident "__llvmMemcpy__", (VFun CompString [CompInt]))
  , (Ident "__concatString__", (VFun CompString [CompString, CompString]))]


initialCompState :: CompState
initialCompState = CompState 0 Map.empty Map.empty predefinedFunctions 0 (-1) Map.empty Map.empty Map.empty "" Map.empty Map.empty Map.empty Map.empty Nothing

type GenM = StateT CompState (ExceptT String IO)

emit :: Instr -> GenM ()
emit instr = do
  CompState currBlock blocks _ _ _ _ _ _ _ _ _ _ _ _ _<- get
  let thisbl = Map.lookup currBlock blocks
  case thisbl of
    Just (CodeBlock instrs label coming terminating) -> do
      let newBlMap = Map.insert currBlock (CodeBlock (instr:instrs) label coming terminating) blocks
      let newBlocks = insertAfterReturn instr (head instrs) blocks newBlMap
      modify $ \curr -> curr {blocks = newBlocks}
    Nothing -> do
      let newBl = CodeBlock [instr] currBlock [] []
      let newBlMap = Map.insert currBlock newBl blocks
      modify $ \curr -> curr {blocks = newBlMap}
  case instr of
    IBranch lb -> addEdge currBlock lb
    IBranchCond _ lb1 lb2 -> do
      addEdge currBlock lb1
      addEdge currBlock lb2
    _ -> return ()

removeEdge :: Int -> Int -> GenM()
removeEdge to from = do
  edgs <- gets edges
  case Map.lookup from edgs of
    Nothing -> return ()
    Just lbEdges -> do
      modify $ \s -> s { edges = Map.insert from (lbEdges L.\\ [to]) edgs }

addEdge :: Int -> Int -> GenM()
addEdge to from = do
  if to == from then return ()
  else do
    edgs <- gets edges
    case Map.lookup from edgs of
      Nothing -> do
        modify $ \s -> s { edges = Map.insert from [to] edgs }
      Just lbEdges -> do
        modify $ \s -> s { edges = Map.insert from (to:lbEdges) edgs }

insertAfterReturn instr headOf defVal updated = case headOf of
  (IRetVal _) -> case instr of
    ICloseBlock -> updated
    _ -> defVal
  IRetVoid -> case instr of
    ICloseBlock -> updated
    _ -> defVal
  _ -> updated

reverseInstructions :: GenM ()
reverseInstructions = do
  cBlock <- gets currBlock
  blck <- gets blocks
  case Map.lookup cBlock blck of
    Just (CodeBlock instrs label cg tg) -> do
      let newBlMap = Map.insert cBlock (CodeBlock (reverse instrs) label cg tg) blck
      modify $ \s -> s { blocks = newBlMap }
    Nothing -> throwError "Looking for non existing block!"

popFirstInstr :: GenM ()
popFirstInstr = do
  cBlock <- gets currBlock
  blck <- gets blocks
  case Map.lookup cBlock blck of
    Just (CodeBlock (ins:instrs) label cg tg) -> do
      let newBlMap = Map.insert cBlock (CodeBlock instrs label cg tg) blck
      modify $ \s -> s { blocks = newBlMap }
    Nothing -> throwError "Looking for non existing block!"

getVar :: Ident -> GenM Address
getVar i = do
  vEnv <- gets vars
  case Map.lookup i vEnv of
    Nothing -> do
      cClass <- gets currClass
      cls <- gets classes
      let (ClassState _ vrs _ _) = cls ! (fromJust cClass)
      return $ vrs ! i
    Just a -> return a

freshTemp :: GenM String
freshTemp = do
  temp <- gets lastReg
  modify $ \s -> s {lastReg = temp + 1}
  return ("%r" ++ show temp)

freshLabel :: GenM Int
freshLabel = do
  temp <- gets lastLabel
  modify $ \s -> s {lastLabel = temp + 1}
  return $ temp + 1

changeLabel :: Int -> GenM ()
changeLabel label = do
  label <- gets lastLabel
  modify $ \s -> s {currBlock = label}

latTypeToLLVM (Int _) = CompInt
latTypeToLLVM (Str _) = CompString
latTypeToLLVM (Bool _) = CompBool
latTypeToLLVM (Void _) = CompVoid
latTypeToLLVM (Class _ (Ident a)) = (CompClass (Ident a))
latTypeToLLVM (Arr _ ct) = (CompArr VUndefined (latTypeToLLVM ct))

argType (Arg _ typ _ ) = latTypeToLLVM typ

fillEnvWithFunctions :: [TopDef] -> GenM ()
fillEnvWithFunctions ((FnDef _ retType name args _):xs) = do
  funs <- gets funcs
  let argTypes = P.map argType args
  modify $ \s -> s {funcs = Map.insert name (VFun (latTypeToLLVM retType) argTypes) funs}
  fillEnvWithFunctions xs

fillEnvWithFunctions [] = return ()

fillEnvWithFunctions ((ClassDef _ id clProps):xs) = do
  clss <- gets classes
  modify $ \s -> s {classes = Map.insert id (ClassState id Map.empty Map.empty Map.empty) clss}
  mapM_ (parseClProps id) clProps
  fillEnvWithFunctions xs

fillEnvWithFunctions ((ClassExtDef _ id ext clProps):xs) = do
  clss <- gets classes
  let (ClassState _ vars funcs _) = clss ! ext
  modify $ \s -> s {classes = Map.insert id (ClassState id vars funcs Map.empty) clss}
  mapM_ (parseClProps id) clProps
  fillEnvWithFunctions xs

parseClProps :: Ident -> ClassProp -> GenM ()
parseClProps id (CAttribute _ typ attrId) = do
  clss <- gets classes
  let (ClassState name vars funcs funcsBlock) = clss ! id
  let newT = latTypeToLLVM typ
  let newVars = Map.insert attrId (newT, VReg (show (Map.size vars))) vars
  modify $ \s -> s { classes = Map.insert id (ClassState name newVars funcs funcsBlock) clss }

parseClProps id (CMethod _ typ methId args bl) = do
  clss <- gets classes
  let (ClassState name vars funcs funcsBlock) = clss ! id
  let newT = latTypeToLLVM typ
  let argTypes = P.map argType args
  let newFuncs = Map.insert methId (VFun newT argTypes) funcs
  let (Ident clName) = id
  let (Ident methName) = methId
  let newFuncsBlock = Map.insert methId (FnDef Nothing typ (Ident $ clName ++ "." ++ methName) args bl) funcsBlock
  modify $ \s -> s { classes = Map.insert id (ClassState name vars newFuncs newFuncsBlock) clss }


removeEmptyBlock = P.map removeEmptyBlockFun
removeEmptyBlockFun ([IPutLabel a]) = []
removeEmptyBlockFun ((IPutLabel a):ICloseBlock:[]) = [ICloseBlock]
removeEmptyBlockFun bl = bl

filterAssigns bl = P.filter (\x -> case x of
  (IAss _ _) -> False
  (IDeclare _) -> False
  _ -> True) bl

printAllBlocks :: GenM ()
printAllBlocks = do
  allBlocks <- gets blocks
  let blocksList = P.map snd (toList allBlocks) -- Todo reverse bl -> filterAssigns bl
  let str = intercalate "" $ P.map (\(CodeBlock bl _ _ _) -> unlines $ P.map (\instr -> showInstr instr) (removeEmptyBlockFun $ filterAssigns $ reverse bl)) blocksList
  code_ <- gets code
  modify $ \s -> s { code = code_ ++ str}

formatString :: String -> String
formatString str = "private constant [" ++ (show $ (length str) + 1) ++ " x i8] c\"" ++ str ++ "\\00\""

printStrings :: GenM ()
printStrings = do
  allStrings <- gets stringsMap
  code_ <- gets code
  let str = unlines $ P.map (\(k, str) -> ("@__string__" ++ show k ++ " = " ++ (formatString str))) (toList allStrings)
  modify $ \s -> s { code = code_ ++ str}

printClasses :: GenM ()
printClasses = do
  clss <- gets classes
  code_ <- gets code
  modify $ \s -> s { code = code_ ++ "\n"}
  mapM_ printClass (P.map snd (Map.toList clss))
  code_2 <- gets code
  modify $ \s -> s { code = code_2 ++ "\n"}
  return ()

printClass :: ClassState -> GenM ()
printClass (ClassState (Ident name) vrs _ _) = do
  code_ <- gets code
  let sorted = P.map fst $ (sortBy (compare `on` snd) (Map.elems vrs)) --ToDo: Double check
  let str = "%class." ++ name ++ " = type {" ++ (intercalate ", " (P.map showCT sorted)) ++ "}\n"
  modify $ \s -> s { code = code_ ++ str}

saveLocals :: GenM ()
saveLocals = do
  vrs <- gets vars
  modify $ \s -> s { localsSave = vrs }
  modify $ \s -> s { bDecl = Map.empty }

bringBackLocals :: GenM ()
bringBackLocals = do
  phs <- gets localsSave
  vrs <- gets vars
  bdcl <- gets bDecl
  modify $ \s -> s { prevVars = vrs }
  modify $ \s -> s { prevBDecl = bdcl }
  modify $ \s -> s { vars = Map.union bdcl phs }

getThisCBVars :: GenM ()
getThisCBVars = do
  cBlock <- gets currBlock
  allBlocks <- gets blocks
  let (CodeBlock instr1 _ _ _) = allBlocks ! cBlock
  let allAss1 = filterSameIdents $ filterAssigns2 instr1
  let idents1 = getIdents allAss1
  let phisListBl1 = getAssesByIdent allAss1 idents1
  mapM_ saveVarFromIAss phisListBl1

saveVarFromIAss :: Instr -> GenM ()
saveVarFromIAss (IAss i1 a1) = do
  vrs <- gets vars
  modify $ \s -> s {vars = Map.insert i1 a1 vrs}


identToString :: Ident -> String
identToString (Ident x) = x

varsMap :: Map Ident (CompType, Val)
varsMap = Map.empty

unsortedArgs :: [Arg] -> [Address]
unsortedArgs = P.map (\(Arg _ typ ident) -> (latTypeToLLVM typ, (VReg ("%" ++ identToString ident))))

argsToVars :: [Arg] -> Map.Map Ident Address
argsToVars args = P.foldl (\mp (Arg _ typ ident) -> Map.insert ident (latTypeToLLVM typ, (VReg ("%" ++ identToString ident))) mp) varsMap args

showCodePlace :: Maybe (Int, Int) -> String
showCodePlace Nothing = ""
showCodePlace (Just (line, pt)) = "At line " ++ show line ++ ", column " ++ show pt ++ ". "


toBlock stmt = case stmt of
  BStmt x y -> BStmt x y
  stmt -> BStmt Nothing (Block Nothing [stmt])

fromBlock (BStmt _ (Block _ stmts)) = stmts

localCmp typ op v1 v2 =
  case typ of
    CompBool -> simpleCmp op v1 v2
    CompInt -> simpleCmp op v1 v2

simpleCmp op v1 v2 = case op of
  LTH _ -> v1 < v2
  LE _ -> v1 <= v2
  GTH _ -> v1 > v2
  GE _ -> v1 >= v2
  EQU _ -> v1 == v2
  NE _ -> not $ v1 == v2

emitPhiBlocks :: Int -> Int -> Int -> GenM ()
emitPhiBlocks blL1 blL2 afterBl = do
  allBlocks <- gets blocks
  cBlock <- gets currBlock

  let (CodeBlock instr1 _ _ _) = allBlocks ! blL1
  let (CodeBlock instr2 _ _ _) = allBlocks ! blL2
  let allAss1 = filterSameIdents $ filterAssigns2 instr1
  let allAss2 = filterSameIdents $ filterAssigns2 instr2

  let idents1 = getIdents allAss1
  let idents2 = getIdents allAss2

  let commonIdents = L.intersect idents1 idents2
  let uniq1 = idents1 L.\\ commonIdents
  let uniq2 = idents2 L.\\ commonIdents

  let phisList = zip (getAssesByIdent allAss1 commonIdents) (getAssesByIdent allAss2 commonIdents)
  let phisListBl1 = getAssesByIdent allAss1 uniq1
  let phisListBl2 = getAssesByIdent allAss2 uniq2

  modify $ \s -> s {currBlock = afterBl}
  mapM_ (emitZippedPhi blL1 blL2) phisList
  mapM_ (emitPhis blL1 blL2) phisListBl1
  mapM_ (emitPhis blL2 blL1) phisListBl2

  lses <- gets localsSave

  modify $ \s -> s {currBlock = cBlock}
  modify $ \s -> s {vars = lses}

filterAssigns2 bl = P.filter (\x -> case x of
  (IAss _ _) -> True
  (IDeclare _) -> True
  _ -> False) bl

nubAss = L.nubBy (\(IAss id1 _) (IAss id2 _) -> id1 == id2)

filterSameIdents list = nubAss $ snd $ P.foldl (\x y -> filterSameIdentsFun y x) ([], []) (reverse list)

filterSameIdentsFun x (banned, resMap) = case x of
  (IAss id1 v) -> case id1 `elem` banned of
    True -> (banned, resMap)
    False -> (banned, (IAss id1 v):resMap)
  (IDeclare id1) -> case id1 `elem` banned of
    True -> (banned, resMap)
    False -> ((id1:banned), resMap)

getIdents x = P.map (\(IAss id _) -> id) x

getAssByIdent id ((IAss i v) : xs) = case (id == i) of
  True -> (IAss i v)
  False -> getAssByIdent id xs

getAssesByIdent instrL x = P.map (\id -> getAssByIdent id (instrL)) x

emitZippedPhi bl1 bl2 ((IAss i1 (typ1, val1)), (IAss i2 (typ2, val2))) = do
  phs <- gets localsSave
  --vrs <- gets vars
  r <- freshTemp
  emit $ IPhi r typ1 [(val1, bl1), (val2, bl2)]
  emit $ IAss i1 (typ1, VReg r)
  modify $ \s -> s {vars = Map.insert i1 (typ1, VReg r) phs}


emitZippedPhi _ _ _ = throwError "FATAL ERROR ON EMITTING PHI"

emitPhis bl1 bl2 (IAss i1 (typ1, val1)) = do
  phs <- gets localsSave
  vrs <- gets vars
  r <- freshTemp
  let (_, currVal) = phs ! i1
  emit $ IPhi r typ1 [(val1, bl1), (currVal, bl2)]
  emit $ IAss i1 (typ1, VReg r)
  modify $ \s -> s {vars = Map.insert i1 (typ1, VReg r) vrs}

emitPhiBlock :: Int -> Int -> GenM ()
emitPhiBlock blL1 afterBl = do
  allBlocks <- gets blocks
  cBlock <- gets currBlock

  let (CodeBlock instr1 _ _ _) = allBlocks ! blL1
  let allAss1 = filterSameIdents $ filterAssigns2 instr1
  let idents1 = getIdents allAss1
  let phisListBl1 = getAssesByIdent allAss1 idents1

  mapM_ (emitPhis blL1 afterBl) phisListBl1
  modify $ \s -> s {currBlock = cBlock}

insertNewReg :: Ident -> GenM ()
insertNewReg id = do
  r <- freshTemp
  vrs <- gets vars
  let (t, _) = vrs ! id
  modify $ \s -> s { vars = Map.insert id (t, VReg r) vrs}

emitWhilePhis :: Map.Map Ident Address -> Map.Map Ident Address -> Int -> Int -> (Ident, Address) -> GenM ()
emitWhilePhis prevMap currMap lb1 lb2 (i, (t, v)) = do
  vrs <- gets vars
  let prevVal = prevMap ! i
  let currVal = currMap ! i
  let (VReg reg) = v
  case (t, v) == currVal of
    True -> do
       modify $ \s -> s {vars = Map.insert i prevVal vrs} --Modify uses of the variable in the block ;_ ;
    False -> do
      emit $ IAss i (t, v) -- ToDo check later for (t, currVal)
      emit $ IPhi reg t [(snd prevVal, lb1), (snd currVal, lb2)]
      modify $ \s -> s {vars = Map.insert i (t, v) vrs}

putRetIfVoid typ = case typ of
    Void _ -> do
      bl <- gets blocks
      cBlock <- gets currBlock
      let (CodeBlock instr lb cm tm) = bl ! cBlock
      case head instr of
         IRetVoid -> return ()
         _ -> emit $ IRetVoid
    _ -> return ()


removeOldVal :: Instr -> Address -> Address -> Instr
removeOldVal instr oldVar newVar = case instr of
  (IRetVal addr) -> (IRetVal $ checkAddresses addr oldVar newVar)
  (ICall maybeReg cTyp (Ident ident) vals) -> (ICall maybeReg cTyp (Ident ident) (P.map (\x -> checkAddresses x oldVar newVar) vals))
  (IBinOp reg op typ a1 a2) -> (IBinOp reg op typ (checkVals a1 (snd oldVar) (snd newVar)) (checkVals a2 (snd oldVar) (snd newVar)))
  (IPhi reg typ poss) -> (IPhi reg typ (P.map (\(x, y) -> (checkVals x (snd oldVar) (snd newVar), y)) poss))
  (IBranchCond v l1 l2) -> (IBranchCond (checkVals v (snd oldVar) (snd newVar)) l1 l2)
  (INot t val) -> (INot t (checkVals val (snd oldVar) (snd newVar)) )
  (ICmp reg typ op a1 a2) -> (ICmp reg typ op (checkAddresses a1 oldVar newVar) (checkAddresses a2 oldVar newVar))
  (IAss id a1) -> (IAss id (checkAddresses a1 oldVar newVar))
  someInstr -> someInstr

checkAddresses :: Address -> Address -> Address -> Address
checkAddresses addr oldVar newVar = if addr == oldVar then newVar else addr

checkVals :: Val -> Val -> Val -> Val
checkVals addr oldVar newVar = if addr == oldVar then newVar else addr

overrideVals :: Int -> Address -> Address -> GenM ()
overrideVals bl oldVar newVar = do
  allBlocks <- gets blocks
  let (CodeBlock instrs label cg tg) = allBlocks ! bl
  let modInstrs = P.map (\x -> removeOldVal x oldVar newVar) instrs
  modify $ \s -> s { blocks = Map.insert bl (CodeBlock modInstrs label cg tg) allBlocks }

overrideValInBlock :: Map.Map Ident Address -> Map.Map Ident Address -> Int -> (Ident, Address) -> GenM ()
overrideValInBlock prevMap currMap lb (i, (t, v)) = do
  vrs <- gets vars
  let prevVal = prevMap ! i
  let currVal = currMap ! i
  case prevVal == currVal of
    True -> do
      overrideVals lb prevVal (t, v)
      vrs <- gets vars
      modify $ \s -> s {vars = Map.insert i (t, v) vrs}
    False -> return ()

checkIfBothEndInReturn :: Int -> Int -> GenM (Bool)
checkIfBothEndInReturn lb1 lb2 = do
  aBlocks <- gets blocks
  let (CodeBlock instr1 _ _ _) = aBlocks ! lb1
  let (CodeBlock instr2 _ _ _) = aBlocks ! lb2
  case (head instr1, head instr2) of
    (IRetVoid, IRetVoid) -> return True
    (IRetVal _, IRetVal _) -> return True
    (_, _) -> return False

assBDecl :: Ident -> GenM ()
assBDecl id = do
  vrs <- gets vars
  bDcl <- gets bDecl
  case Map.lookup id vrs of
    Nothing -> return ()
    Just (val) -> modify $ \s -> s { bDecl = Map.insert id val bDcl }


data OptInstr = OptIBinOp BinOp CompType Val Val
  | OptIPhi CompType [(Val, Int)]
  | OptINot Val
  | OptICmp CompType RelOp Address Address
  | OptICmpString Address Address
  | NotOpt
  deriving (Show, Eq, Ord)

lsce :: GenM ()
lsce = do
  allBlocks <- gets blocks
  mapM_ lsceBlock (Map.keys allBlocks)

lsceBlock :: Int -> GenM ()
lsceBlock id = do
  allBlocks <- gets blocks
  let (CodeBlock instrs lb cm tr) = allBlocks ! id
  modify $ \s -> s {currBlock = id}
  newIstrs <- optInstrs (reverse $ filterAssigns instrs)
  modify $ \s -> s { blocks = Map.insert id (CodeBlock (reverse newIstrs) lb cm tr) allBlocks }

optInstrs :: Instrs -> GenM (Instrs)
optInstrs instrs = do
   opted <- optIteration instrs Map.empty []
   if opted == instrs then return opted
   else do
     res <- optInstrs opted
     return res

optIteration :: Instrs -> Map.Map OptInstr String -> Instrs -> GenM (Instrs)
optIteration [] _ res = return $ reverse res
optIteration (i:instrs) optMap res = do
  let optType = toOptT i
  case Map.lookup optType optMap of
    Nothing -> do
       goFurther <- optIteration instrs (updateOptMap i optMap) (i:res)
       return $ goFurther
    Just (newReg) -> do
      let oldReg = getRegFrom i
      newVal <- optIteration (P.map (\x -> swapReg oldReg (VReg newReg) x) instrs ) optMap ((swapReg oldReg (VReg newReg) i):res)
      return $ newVal


updateOptMap :: Instr -> Map.Map OptInstr String -> Map.Map OptInstr String
updateOptMap instr optMap = case instr of
  IBinOp reg op vType v1 v2 -> case op of
     (BAdd) -> Map.union (Map.fromList [((OptIBinOp BAdd vType v2 v1), reg), ((OptIBinOp BAdd vType v1 v2), reg)]) optMap
     (BMul) -> Map.union (Map.fromList [((OptIBinOp BMul vType v2 v1), reg), ((OptIBinOp BMul vType v1 v2), reg)]) optMap
     _ -> Map.insert (OptIBinOp op vType v1 v2) reg optMap
     --               OptIBinOp BinOp CompType Val Val
  IPhi reg cT phis -> Map.insert (OptIPhi cT (L.sort phis)) reg optMap
  INot reg val -> Map.insert (OptINot val) reg optMap
  ICmp reg cT op a1 a2 -> case op of
    LTH _ -> Map.union (Map.fromList [((OptICmp cT (LTH Nothing) a1 a2), reg), ((OptICmp cT (GTH Nothing) a2 a1), reg)]) optMap
    LE _ -> Map.union (Map.fromList [((OptICmp cT (LE Nothing) a1 a2), reg), ((OptICmp cT (GE Nothing) a2 a1), reg)]) optMap
    GTH _ -> Map.union (Map.fromList [((OptICmp cT (GTH Nothing) a1 a2), reg), ((OptICmp cT (LTH Nothing) a2 a1), reg)]) optMap
    GE _ -> Map.union (Map.fromList [((OptICmp cT (GE Nothing) a1 a2), reg), ((OptICmp cT (LE Nothing) a2 a1), reg)]) optMap
    EQU _ -> Map.union (Map.fromList [((OptICmp cT (EQU Nothing) a1 a2), reg), ((OptICmp cT (EQU Nothing) a2 a1), reg)]) optMap
    NE _ -> Map.union (Map.fromList [((OptICmp cT (NE Nothing) a1 a2), reg), ((OptICmp cT (NE Nothing) a2 a1), reg)]) optMap
  _ -> optMap

-- OptICmp CompType RelOp Address Address
-- ICmp String CompType RelOp Address Address

swapAddr :: Val -> Val -> Address -> Address
swapAddr oldReg newReg (t, v) = if v == oldReg then (t, newReg) else (t, v)

locSwapReg :: Val -> Val -> Val -> Val
locSwapReg oldReg newReg v = if v == oldReg then newReg else v

stringSwap :: Val -> Val -> String -> String
stringSwap (VReg oldReg) (VReg newReg) v = if v == oldReg then newReg else v

swapInPhi oldReg newReg (val, int) = (locSwapReg oldReg newReg val, int)

swapReg :: Val -> Val -> Instr -> Instr
swapReg oldReg newReg i = case i of
  (IRetVal addr) -> IRetVal (swapAddr oldReg newReg addr)
  (ICall maybeReg cTyp (Ident ident) vals) -> (ICall maybeReg cTyp (Ident ident) (P.map (swapAddr oldReg newReg) vals))
  (IBinOp reg op typ a1 a2) -> (IBinOp reg op typ (locSwapReg oldReg newReg a1) (locSwapReg oldReg newReg a2))
  (IPhi reg typ poss) -> (IPhi reg typ (P.map (swapInPhi oldReg newReg) poss))
  (IBranchCond v l1 l2) -> (IBranchCond (locSwapReg oldReg newReg v) l1 l2)
  (INot t val) -> (INot t (locSwapReg oldReg newReg val))
  (ICmp reg typ op a1 a2) -> (ICmp reg typ op (swapAddr oldReg newReg a1) (swapAddr oldReg newReg a2))
  (ICmpString reg a1 a2) -> (ICmpString reg (swapAddr oldReg newReg a1) (swapAddr oldReg newReg a2))
  (IGetElemPtr reg a1 a2 a3) -> (IGetElemPtr reg (swapAddr oldReg newReg a1) (swapAddr oldReg newReg a2) (swapAddr oldReg newReg a3))
  (IGetElemPtrClass reg a1 a2 a3 a4) -> (IGetElemPtrClass reg (swapAddr oldReg newReg a1) (swapAddr oldReg newReg a2) (swapAddr oldReg newReg a3) (swapAddr oldReg newReg a4))
  (IPtrToInt reg a) -> (IPtrToInt reg (swapAddr oldReg newReg a))
  (IBitCast r1 a t) -> (IBitCast r1 (swapAddr oldReg newReg a) t)
  _ -> i

getRegFrom :: Instr -> Val
getRegFrom i = case i of
    IBinOp str _ _ _ _ -> VReg str
    IPhi str _ _ -> VReg str
    INot str _ -> VReg str
    ICmp str _ _ _ _ -> VReg str
    ICmpString str _ _ -> VReg str

toOptT :: Instr -> OptInstr
toOptT instr = case instr of
  IBinOp reg op vType v1 v2 -> (OptIBinOp op vType v1 v2)
  IPhi reg cT phis -> (OptIPhi cT (L.sort phis))
  INot reg val -> (OptINot val)
  ICmp reg cT op a1 a2 -> OptICmp cT (plToNothing op) a1 a2
  i -> NotOpt

plToNothing :: RelOp -> RelOp
plToNothing (LTH _) = (LTH Nothing)
plToNothing (LE _) = (LE Nothing)
plToNothing (GTH _) = (GTH Nothing)
plToNothing (GE _) = (GE Nothing)
plToNothing (EQU _) = (EQU Nothing)
plToNothing (NE _) = (NE Nothing)

getInstrsFromBl :: CodeBlock -> Instrs
getInstrsFromBl (CodeBlock instrs _ _ _) = instrs

cmpAllBlocks :: Map.Map Int CodeBlock -> Map.Map Int CodeBlock -> Bool
cmpAllBlocks bl1 bl2 = do
  let allIntrs1 = P.map getInstrsFromBl $ elems bl1
  let allIntrs2 = P.map getInstrsFromBl $ elems bl2
  P.all (\(x, y) -> x == y) $ P.zip allIntrs1 allIntrs2

genOptMapInstr :: Instrs -> Map.Map OptInstr String -> Instrs -> Int -> GenM ()
genOptMapInstr [] _ res _ = return ()

genOptMapInstr (i:instrs) optMap res curr = do
  let optType = toOptT i
  case Map.lookup optType optMap of
    Nothing -> do
       goFurther <- genOptMapInstr instrs optMap (i:res) curr
       return $ goFurther
    Just (newReg) -> do
      let oldReg = getRegFrom i
      if oldReg == (VReg newReg) then do
        genOptMapInstr instrs optMap (i:res) curr
      else do
        bfsReg [] [curr] oldReg (VReg newReg)
        genOptMapInstr (P.map (\x -> swapReg oldReg (VReg newReg) x) instrs ) optMap ((swapReg oldReg (VReg newReg) i):res) curr


bfsReg :: [Int] -> [Int] -> Val -> Val -> GenM ()
bfsReg _ [] _ _ = return ()
bfsReg seen queue oldReg newReg = do
  let curr = head queue
  let newSeen = (curr:seen)
  blEdg <- getBlockEdges curr
  let newQueue = (L.nub (queue ++ blEdg)) L.\\ newSeen
  allBl <- gets blocks
  let (CodeBlock instrs l1 l2 l3) = allBl ! curr
  let newInstrs = P.map (\x -> swapReg oldReg newReg x) instrs
  modify $ \s -> s {blocks = Map.insert curr (CodeBlock newInstrs l1 l2 l3) allBl}
  --lsceBlock curr -- ToDo <- Double check...Looks pretty nasty
  bfsReg newSeen newQueue oldReg newReg

getBlockEdges :: Int -> GenM ([Int])
getBlockEdges lb = do
  edg <- gets edges
  case Map.lookup lb edg of
    (Just e) -> return e
    (Nothing) -> return []

isSamePoss poss = do
  let cases = L.nub $ P.map fst poss
  case cases of
    [e] -> Just e
    _ -> Nothing

removeIfUnusedPhi :: Int -> Instr -> GenM ()
removeIfUnusedPhi curr i = do
  case i of
    (IPhi reg _ poss) -> do
      case isSamePoss poss of
        Nothing -> return ()
        Just (newReg) -> do
           bfsReg [] [curr] (VReg reg) (newReg)
    _ -> return ()

removePhis :: Int -> GenM ()
removePhis curr = do
  allBl <- gets blocks
  let (CodeBlock instrs _ _ _) = allBl ! curr
  mapM_ (removeIfUnusedPhi curr) instrs

removeUnusedRegs :: GenM ()
removeUnusedRegs = do
  allBlocks <- gets blocks
  mapM_ removeUnusedRegsIt (Map.keys allBlocks)

removeUnusedRegsIt :: Int -> GenM ()
removeUnusedRegsIt id = do
  allBlocks <- gets blocks
  let (CodeBlock instr l1 l2 l3) = allBlocks ! id
  newInstrs <- removeUnusedInstrs instr []
  modify $ \s -> s {blocks = Map.insert id (CodeBlock newInstrs l1 l2 l3) allBlocks}
  return ()

removeUnusedInstrs :: Instrs -> Instrs -> GenM (Instrs)
removeUnusedInstrs [] res = return $ reverse res
removeUnusedInstrs (i:instrs) res = do
  case getRegOfInstr i of
    Nothing -> do
       res <- removeUnusedInstrs (instrs) (i:res)
       return res
    (Just reg) -> do
      allInstrs <- getAllInstrs
      if P.any (isRegUsed reg) allInstrs then do
        res <- removeUnusedInstrs instrs (i:res)
        return res
      else do
       res <- removeUnusedInstrs instrs res
       return res

getRegOfInstr i = case i of
  (ICall maybeReg _ _ _) -> maybeReg
  (IBinOp reg _ _ _ _) -> Just (VReg reg)
  (IPhi reg _ _) -> Just (VReg reg)
  (INot t _) -> Just (VReg t)
  (ICmp reg _ _ _ _) -> Just (VReg reg)
  (ICmpString reg _ _) -> Just (VReg reg)
  _ -> Nothing

cmpRegAddr reg (_, reg2) = reg == reg2

isRegUsed reg i = case i of
  (IRetVal addr) -> cmpRegAddr reg addr
  (ICall _ _ _ addresses) -> P.any (cmpRegAddr reg) addresses
  (IBinOp _ _ _ v1 v2) -> reg == v1 || reg == v2
  (IBranchCond v _ _) -> reg == v
  (IPhi _ _ possVals) -> P.any (== reg) (P.map fst possVals)
  (INot _ v) -> reg == v
  (IDecFun _ _ addresses) -> P.any (cmpRegAddr reg) addresses
  (ICmp _ _ _ a1 a2) -> cmpRegAddr reg a1 || cmpRegAddr reg a2
  (ICmpString _ a1 a2) -> cmpRegAddr reg a1 || cmpRegAddr reg a2
  (IGetElemPtr _ a1 a2 a3) -> P.any (cmpRegAddr reg) [a1, a2, a3]
  (IGetElemPtrClass _ a1 a2 a3 a4) -> P.any (cmpRegAddr reg) [a1, a2, a3, a4]
  (IPtrToInt _ a) -> cmpRegAddr reg a
  (IBitCast _ a _) -> cmpRegAddr reg a
  _ -> False

getInstrsFromCB (CodeBlock instr _ _ _) = instr

getAllInstrs :: GenM (Instrs)
getAllInstrs = do
    allBl <- gets blocks
    return $ P.concat $ P.map getInstrsFromCB (P.map snd (Map.toList allBl))


gsce2 :: GenM()
gsce2 = do
  allBlocks <- gets blocks
  edg <- gets edges
  mapM_  gsce2Block (Map.keys edg)
  newBlock <- gets blocks
  modify $ \s -> s {edges = edg}
  if cmpAllBlocks allBlocks newBlock
    then return ()
    else do gsce2

gsce2Block :: Int -> GenM()
gsce2Block lb = do
  (optMap, regs) <- getRedundancies lb
  modifyBlockWithMap optMap lb
  modifyBlockWithRegList regs lb
  removePhis lb
  return ()
  -- mapM_ swapRegs
  --Do block optimisation like in

getRedundancies :: Int -> GenM (Map.Map OptInstr String,  [(Val, Val)])
getRedundancies curr = do
  blEdg <- getBlockEdges curr
  case blEdg of
    [] -> do
      redundancies <- generateRedundancies curr
      return redundancies
    [a] -> do
      (mp2, l2) <- getRedundancies a
      modifyBlockWithMap mp2 curr
      removableRegs <- mapToRegsRedundancies mp2 curr
      let allRegs = removableRegs ++ l2

      modifyBlockWithRegList allRegs curr
      (mp1, l1) <- generateRedundancies curr
      return (Map.union mp2 mp1, l1 ++ allRegs)
    [a, b] -> do
      (mp1, l1) <- getRedundancies a
      (mp2, l2) <- getRedundancies b
      let interMap = Map.intersection mp1 mp2
      let allList = l1 ++ l2
      removableRegs <- mapToRegsRedundancies interMap curr
      let allRegs = removableRegs ++ allList
      modifyBlockWithMap interMap curr
      modifyBlockWithRegList allRegs curr
      (mpC, lC) <- generateRedundancies curr
      return (Map.union interMap mpC, allRegs ++ lC)
    _ -> throwError $ "3 or more edges in block? \n"

generateRedundancies :: Int -> GenM (Map.Map OptInstr String,  [(Val, Val)])
generateRedundancies curr = do
  allBlocks <- gets blocks
  let (CodeBlock instr1 _ _ _) = allBlocks ! curr
  let redundancies = createRedundancies (reverse instr1) Map.empty []
  return redundancies

createRedundancies [] optMap l = (optMap, l)
createRedundancies (i:instrs) optMap l = do
  let optType = toOptT i
  case Map.lookup optType optMap of
    Nothing -> createRedundancies instrs (updateOptMap i optMap) l
    Just (newReg) -> do
      let oldReg = getRegFrom i
      createRedundancies instrs optMap ((oldReg, (VReg newReg)):l)

modifyBlockWithMap :: Map.Map OptInstr String -> Int -> GenM ()
modifyBlockWithMap interMap curr = do
  allBlocks <- gets blocks
  let (CodeBlock instr1 l1 l2 l3) = allBlocks ! curr
  let modifiedInstrs = modifyBlockWithMapIter (reverse instr1) interMap []
  modify $ \s -> s {blocks = Map.insert curr (CodeBlock modifiedInstrs l1 l2 l3) allBlocks }

modifyBlockWithMapIter [] _ res = res
modifyBlockWithMapIter (i:instrs) optMap res = do
  let optType = toOptT i
  case Map.lookup optType optMap of
    Nothing -> modifyBlockWithMapIter instrs optMap (i:res)
    Just (newReg) -> do
      let oldReg = getRegFrom i
      modifyBlockWithMapIter (P.map (\x -> swapReg oldReg (VReg newReg) x) instrs ) optMap ((swapReg oldReg (VReg newReg) i):res)


mapToRegsRedundancies :: Map.Map OptInstr String -> Int -> GenM ([(Val, Val)])
mapToRegsRedundancies interMap curr = do
  allBlocks <- gets blocks
  let (CodeBlock instr1  _ _ _) = allBlocks ! curr
  let regs = getRegsReduncancies (reverse instr1) interMap []
  return regs

getRegsReduncancies [] _ res = res
getRegsReduncancies (i:instrs) optMap res = do
  let optType = toOptT i
  case Map.lookup optType optMap of
    Nothing -> case i of
      (IPhi reg _ poss) -> do
        case isSamePoss poss of
          Nothing -> getRegsReduncancies instrs optMap res
          Just (newReg) -> getRegsReduncancies instrs optMap ((VReg reg, newReg):res)
      _ -> getRegsReduncancies instrs optMap (res)
    Just (newReg) -> do
      let oldReg = getRegFrom i
      getRegsReduncancies instrs optMap ((oldReg, VReg newReg):res)

modifyBlockWithRegList :: [(Val, Val)] -> Int -> GenM ()
modifyBlockWithRegList allRegs curr = do
  allBlocks <- gets blocks
  let (CodeBlock instr1 l1 l2 l3) = allBlocks ! curr
  let modifiedInstrs = modifyBlockWithRegListIter (reverse instr1) allRegs []
  modify $ \s -> s {blocks = Map.insert curr (CodeBlock modifiedInstrs l1 l2 l3) allBlocks }

modifyBlockWithRegListIter [] _ res = res
modifyBlockWithRegListIter (i:intrs) regs res = modifyBlockWithRegListIter intrs regs ((iterChanges i regs):res)

iterChanges i [] = i
iterChanges i ((oldReg, newReg):xs) = do
  let newInstr = swapReg oldReg newReg i
  if (newInstr == i) then
    iterChanges i xs
  else
    newInstr

getClass :: Ident -> GenM (ClassState)
getClass className = do
  clss <- gets classes
  case Map.lookup className clss of
    Just cs -> return cs
    Nothing -> throwError $ "Class name is not defined" ++ showIdent className

getClassVars :: Ident -> GenM (Map.Map Ident Address)
getClassVars className = do
  (ClassState _ vrs _ _) <- getClass className
  return vrs

getClassVar :: Ident -> Ident -> GenM (Maybe Address)
getClassVar className var = do
   vrs <- getClassVars className
   return $ Map.lookup var vrs