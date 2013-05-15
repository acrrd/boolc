 {-# LANGUAGE FlexibleContexts #-}

module ToLLVM where

import Ast
import qualified Typesystem as T
import qualified Types as T

import LLVM hiding (not,div,rem)
import qualified LLVM as L
import qualified LLVM.FFI.Core as FFI

import Data.Char (isDigit)
import Data.List ((\\),union)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Foreign.Marshal.Array (withArrayLen)
import Foreign.C.String (peekCString,withCString,newCString)

import Debug.Trace

genLLVMCode :: T.ProgramT a -> T.ClassTypeEnv -> FilePath -> IO ()
genLLVMCode p cte output = do
  modul <- createModule output
  runCodeGenModule modul genModule
  writeBitcodeToFile output modul

  where genModule = do
          sci <- staticClassInfos
          genProgram p cte sci "Main" "main"


type FieldOffSets = M.Map FieldName Int
type MethodOffSets = M.Map MethodName Int
type MethodsTable = M.Map MethodName Value
data ClassInfo = ClassInfo { classType :: Type,
                             parentName :: ClassName,
                             constr :: Value,
                             fieldOffsets :: FieldOffSets,
                             methodOffsets :: MethodOffSets
                           } deriving Show

lift3 f = (lift2 . lift) f
lift4 f = (lift3 . lift) f 

builtInClassInfoObject :: Type -> CodeGenModule ClassInfo
builtInClassInfoObject ct = do
  let hierarchyt = FFI.pointerType (FFI.pointerType FFI.int8Type 0) 0
  let ctnamet = (FFI.pointerType FFI.int8Type 0)
  lift $ structSetBody ct [hierarchyt,ctnamet] False
  kv <- makeConstr ct
  return $ ClassInfo ct "" kv filedoffset methodoffset

  where filedoffset = M.fromList $ [(".hierarchy",0),(".ctname",1)]
        methodoffset = M.empty
        makeConstr ct = do let hierarchyt = FFI.pointerType (FFI.pointerType FFI.int8Type 0) 0
                           kt <- lift $ functionType False FFI.voidType [FFI.pointerType ct 0]
                           kv <- newNamedFunction FFI.ExternalLinkage (genConstrSym "Object") kt []
                           defineFunction kv $ \params -> do
                             let thisv = snd $ params !! 0
                             scnv <- addGlobalString "Object" ".str"
                             cnv <- gep thisv [0,1] ".ctname"
                             store scnv cnv
                             av <- genHierarchy "Object" ["Object"] 
                             h <-  gep thisv [0,0] ".hierarchy"
                             av' <- bitCast av $ hierarchyt
                             store av' h
                             retVoid
                           return kv

builtInClassInfoMap :: M.Map ClassName (Type -> CodeGenModule ClassInfo)
builtInClassInfoMap = M.insert "Object" (builtInClassInfoObject) M.empty

type SignatureMap = M.Map [T.Type] Value
type StaticMethodsTable = M.Map MethodName SignatureMap
type StaticClassInfos = M.Map ClassName StaticMethodsTable

staticClassInfos :: CodeGenModule StaticClassInfos
staticClassInfos = do
  system <- buildSystemStaticClassInfo
  return $ M.insert "System" system M.empty

buildSystemStaticClassInfo :: CodeGenModule StaticMethodsTable
buildSystemStaticClassInfo = do
  srandsigs <- srand
  randsigs <- rand
  (printssig,printlnssig) <- prints
  runtime_castcheckv <- runtime_castcheck
  
  let m = [("srand",srandsigs),("rand",randsigs),
           ("print",printssig),("println",printlnssig),
           (".runtime_castcheck",runtime_castcheckv)
          ]
  return $ M.fromList m

  where
    me = M.empty
    runtime_castcheck = do
      let stringt = (FFI.pointerType FFI.int8Type 0)
      {--
      let strcmprt = FFI.int32Type
      
      strcmpt <- lift $ functionType False randrt [stringt,stringt]
      strcmpv <- newNamedFunction FFI.ExternalLinkage ("strcmp") strcmp []
      --}
      let checkrt = FFI.voidType
      checkt <- lift $ functionType False checkrt [stringt,FFI.pointerType stringt 0,FFI.int32Type]
      checkv <- newNamedFunction FFI.ExternalLinkage ("runtime_castcheck") checkt []
      return $ M.insert [] checkv me
    rand = do
      let randrt = FFI.int32Type
      randt <- lift $ functionType False randrt []
      randv <- newNamedFunction FFI.ExternalLinkage ("rand") randt []
      return $ M.insert [] randv me
    srand = do
      let srandrt = FFI.voidType
      srandt <- lift $ functionType False srandrt [FFI.int32Type]
      srandv <- newNamedFunction FFI.ExternalLinkage ("srand") srandt []

      let wrapsrandrt = FFI.voidType
      wrapsrandt <- lift $ functionType False wrapsrandrt []
      wrapsrandv <- newNamedFunction FFI.ExternalLinkage ("wrapsrand") wrapsrandt []

      defineFunction wrapsrandv $ \_ -> do
        r <- tmpVal $ alloca FFI.int32Type
        r' <- load r
        tmpVal $ call srandv [r']
        retVoid

      return $ M.insert [] wrapsrandv me
    prints = do
      let stringt = (FFI.pointerType FFI.int8Type 0)
      let printfrt = FFI.int32Type
      printft <- lift $ functionType True printfrt [stringt]
      printf <- newNamedFunction FFI.ExternalLinkage ("printf") printft []
      
      let buildPrints' = buildPrints printf
      liftM2 (,) (buildPrints' "" "") (buildPrints' "Ln" "\n")

    buildPrints printf namepfs formatpfs = do
      let stringt = (FFI.pointerType FFI.int8Type 0)
      let printWithFormat' = printWithFormat printf
      let idconv = \v -> return v
      let boolconv = \v -> do truev <- addGlobalString "True" ".str"
                              falsev <- addGlobalString "False" ".str"
                              tmpVal $ select v truev falsev

      foldM (\map (sig,name,t,format,conv) -> do
                v <- printWithFormat' name t format conv
                return $ M.insert [sig] v map
            ) M.empty $ map (makePrintTuple namepfs formatpfs) [(T.TInt,"printInt",FFI.int32Type,"%d",idconv),
                                                                (T.TString,"printString",stringt,"%s",idconv),
                                                                (T.TBool,"printBool",FFI.int1Type,"%s",boolconv)
                                                               ]

      where printWithFormat :: Value -> String -> Type -> String -> (Value -> CodeGenFunction Value) -> CodeGenModule Value
            printWithFormat baseprint name t format conv = do
              let printrt = FFI.voidType
              printt <- lift $ functionType False printrt [t]
              printv <- newNamedFunction FFI.ExternalLinkage (name) printt []
              --addAttribute printfIntv AlwaysInlineAttribute
              defineFunction printv $ \params -> do
                formatv <- addGlobalString format ".str"
                v <- conv $ snd $ params!!0
                tmpVal $ call baseprint [formatv,v]
                retVoid
              return printv

            makePrintTuple namepfs formatpfs (sig,name,t,format,conv) = (sig,name++namepfs,t,format++formatpfs,conv)
            
type2LLVMType :: T.Type -> ClassInfoEnvComp Type
type2LLVMType T.TInt = return FFI.int32Type
type2LLVMType T.TBool = return  FFI.int1Type
type2LLVMType T.TString = return $ FFI.pointerType FFI.int8Type 0
type2LLVMType T.TNull = error "No one should ask for TNull"
type2LLVMType (T.TObjId cn) = do env <- ask
                                 case M.lookup cn env of
                                   Nothing -> error $ cn ++ " does not exist"
                                   Just ci -> return $ FFI.pointerType (classType ci) 0
type2LLVMType (T.TRef t) = type2LLVMType t
type2LLVMType T.TVoid = return FFI.voidType

genClassNameSym :: String -> String
genClassNameSym = (".struct." ++)

genMethodNameSym :: String -> String -> String
genMethodNameSym cn = ((".method." ++ cn) ++) . ("." ++)

genConstrSym :: String -> String
genConstrSym = (".constr." ++)

genUserVarSym :: String -> String
genUserVarSym s = ".u." ++ s ++ "."

genInternalVarSym :: String -> String
genInternalVarSym s = ".i." ++ s ++ "."

genMemberAccessSym :: String -> String -> String
genMemberAccessSym tn mn = 
  let prefix = ".m" 
      tn' = if not $ null tn then if last tn == '.' then tn  else tn ++ "."
                             else ""                                     
      base = tn' ++ mn 
  in
    if prefix == take (length prefix) tn then base
                                         else prefix ++ base     

tmpVal :: (String -> a) -> a
tmpVal = ($ ".t")

type ClassInfoEnv = (M.Map ClassName ClassInfo)
type BuildClassInfoEnvComp = ReaderT T.ClassTypeEnv (StateT (ClassInfoEnv,S.Set ClassName) CodeGenModule)
type ClassInfoEnvComp = ReaderT (ClassInfoEnv) IO


buildClassInfoEnv :: T.ClassTypeEnv -> CodeGenModule ClassInfoEnv
buildClassInfoEnv cte = do 
  bcie <- lift $ buildBaseCIE cte
  (cie,_) <- execStateT (runReaderT (mapM_ buildCIE (M.keys cte)) cte) $ (bcie,S.empty)
  return cie

 where buildCIE :: ClassName -> BuildClassInfoEnvComp ()
       buildCIE cn = do
         (cie,visited) <- get
         if S.member cn visited then return ()
           else do 
             cte <- ask
             case M.lookup cn cte of
               Nothing -> error "ClassName taken from cte keys is not in cte"
               Just (T.CT _ pn fns ftm mtm mods) -> do
                 if any (==T.Static) mods then do let cie' = M.delete cn cie
                                                  let visited' = S.insert cn visited
                                                  put (cie',visited')
                   else do
                     case M.lookup cn cie of
                       Nothing -> error $ "Malformed cie. Cannot find " ++ cn ++ " in cie"
                       Just ci -> do
                         case M.lookup cn builtInClassInfoMap of
                           Just cic -> do ci <- lift2 $ cic $ classType ci
                                          let cie' = M.insert cn ci cie
                                          let visited' = S.insert cn visited
                                          put (cie',visited')
                           Nothing -> do
                             when (not $ S.member pn visited) $ buildCIE pn -- assert not $ null pn
                             (cie,visited) <- get
                             case M.lookup pn cie of
                               Nothing -> error "Parent info must be in the environment"
                               Just pi -> do let ct = classType ci
                                             let pt = classType pi
                                             let pmos = methodOffsets pi
                                             pmns <- getParentsMethodNames pn
                                             let pmnsm = M.fromList $ zip pmns [0..]
                                             structbody <- buildStructBody ct fns ftm (mtm `M.difference` pmnsm)
                                             let (fos,mos) = makeOffsets fns (M.keys mtm \\ pmns)
                                             lift3 $ structSetBody ct (pt:structbody) False
                                             kv <- buildConstr cn ct

                                             let ci = ClassInfo (ct) pn kv fos mos
                                             let cie' = M.insert cn ci cie
                                             let visited' = S.insert cn visited
                                             put (cie',visited')

       buildBaseCIE :: T.ClassTypeEnv -> IO ClassInfoEnv
       buildBaseCIE cte = liftM  M.fromList (mapM makePair $ M.keys cte)

       makePair :: ClassName -> IO (ClassName,ClassInfo)
       makePair k = do t <- structCreateNamed $ genClassNameSym k
                       return $ (k,ClassInfo t "" (FFI.constNull FFI.int1Type) M.empty M.empty)

       getParentsMethodNames :: ClassName -> BuildClassInfoEnvComp [MethodName]
       getParentsMethodNames cn = do
         if null cn then return []
           else do
             (cie,_) <- get
             ci <- case M.lookup cn cie of
               Nothing -> error "Class is not in classinfo environmet"
               Just ci -> return ci

             pmns <- getParentsMethodNames $ parentName ci
             return $ (M.keys $ methodOffsets ci) `union` pmns

       buildStructBody :: Type -> [FieldName] -> T.FieldsType -> T.MethodsType -> BuildClassInfoEnvComp [Type]
       buildStructBody ct fns ftm mtm = do 
         (cie,_) <- get
         sft <- mapM (\fn -> case M.lookup fn ftm of
                               Nothing -> error "FieldName must be in the map"
                               Just t -> lift3 $ runReaderT (type2LLVMType t) cie
                     ) fns 
         smt <- mapM (\mn -> case M.lookup mn mtm of
                               Nothing -> error "MethodName must be in the map"
                               Just (ptss,rt) -> do -- We handle only the first signature
                                                    -- Multiple signature method should be only in static class
                                                    mt <- lift3 $ makeMethodType cie ct (ptss !! 0) rt
                                                    let mt' = FFI.pointerType mt 0
                                                    return mt'
                     ) $ M.keys mtm
         return $ sft ++ smt

       makeOffsets :: [FieldName] -> [MethodName] -> (FieldOffSets,MethodOffSets)
       makeOffsets fns mns = (fos,mos)
         where fosmin = 1
               fosmax = fosmin + length fns
               mosmax = fosmax + length mns
               fos = M.fromList $ zip fns [fosmin..fosmax]
               mos = M.fromList $ zip mns [fosmax..mosmax]

       buildConstr :: ClassName -> Type -> BuildClassInfoEnvComp Value
       buildConstr cn ct  =  do
            (cie,_) <- get
            fns <- case runReaderT (T.getContrFields () cn) cte of
                     Left _ -> error "Internal error while getting the constructr parameters"
                     Right fns -> return fns
            fts <- case runReaderT (T.getContructorType () cn) cte of
                     Left _ -> error "Internal error while getting the constructr parameters type"
                     Right fts -> return fts
            kt <- lift3 $ makeMethodType cie ct fts T.TVoid
            lift2 $ newNamedFunction FFI.ExternalLinkage (genConstrSym cn) kt $ map genUserVarSym ("this":fns)

type VarMap = M.Map VarName Value
type LLVMCodeGenMethod = ReaderT (ClassInfoEnv,StaticClassInfos) CodeGenModule
type LLVMCodeGen = StateT VarMap (ReaderT (ClassInfoEnv,StaticClassInfos) CodeGenFunction)

deSymParametersName = map (\(n,v) -> (deSymParameterName n,v))
deSymParameterName s = let suf = drop ((length $ genUserVarSym "") -1) s
                           pre = take (length suf - 1) suf in pre

makeMethodType :: ClassInfoEnv -> Type -> [T.Type] -> T.Type -> IO Type
makeMethodType cie ct pts rt = do
  rt' <- t2LT rt
  pts' <- mapM t2LT $ pts
  let ct' = FFI.pointerType (ct) 0
  functionType False rt' (ct':pts')

  where t2LT t = runReaderT (type2LLVMType t) cie

runLLVMCodeGenMethod :: (ClassInfoEnv,StaticClassInfos) -> LLVMCodeGenMethod a -> CodeGenModule a
runLLVMCodeGenMethod env body = runReaderT body env

runLLVMCodeGen :: VarMap -> (ClassInfoEnv,StaticClassInfos) -> LLVMCodeGen a -> CodeGenFunction a
runLLVMCodeGen vm env body = runReaderT (evalStateT body vm) env

getClassInfo :: ClassInfoEnv -> ClassName -> IO ClassInfo
getClassInfo cie cn = do
  case M.lookup cn cie of
    Nothing -> error "Class is not in environment"
    Just ci -> return ci

getMethodParamTypes :: Value -> LLVMCodeGen [Type]
getMethodParamTypes pv = do
  ft <- lift4 $ getFunctionType pv
  lift4 $ getFunctionParamTypes ft

getField :: Value -> ClassName -> FieldName -> LLVMCodeGen Value
getField = getMember fieldOffsets

getMethod :: Value -> ClassName -> MethodName -> LLVMCodeGen Value
getMethod = getMember methodOffsets

getMember :: (ClassInfo -> M.Map String Int) -> Value -> ClassName -> String -> LLVMCodeGen Value
getMember getter this cn mn = do
  indexs <- getMemberIndex getter cn mn [0]
  thisname <- lift2 $ getValueNameRaw this
  let name = genMemberAccessSym thisname mn
  lift $ lift $ gep this indexs name

  where getMemberIndex :: (ClassInfo -> M.Map String Int) -> ClassName -> String -> [Int] -> LLVMCodeGen [Int]
        getMemberIndex getter cn mn index = do
          when (null cn) $ error $ "Member ("++mn++") not found"
          (cie,_) <- ask
          ci <- case M.lookup cn cie of
            Nothing -> error "Class is not in environment"
            Just ci -> return ci
  
          case M.lookup mn $ getter ci of
            Nothing -> getMemberIndex getter (parentName ci) mn $ index ++ [0] -- parent is always the first
            Just offset -> return $ index ++ [offset]

getVarValue :: VarName -> LLVMCodeGen Value
getVarValue vn = do
  varmap <- get
  case M.lookup vn varmap of
    Nothing -> error "Variabile is not in environment"
    Just v -> return v

genProgram :: T.ProgramT a 
           -> T.ClassTypeEnv -> StaticClassInfos 
           -> ClassName -> MethodName 
           -> CodeGenModule ()
genProgram (Program cds) cte sci cn mn = do
  cie <- buildClassInfoEnv cte
  let env = (cie,sci)
  runLLVMCodeGenMethod env (mapM_ (genClass cte) cds)
  when (M.member cn cte) $ do
    fs <- case runReaderT (T.getContrFields () cn) cte of
          Left _ -> error "Internal error while getting class fields"
          Right fs -> return fs
    (T.CT _ _ _ _ ms _) <- case runReaderT (T.getClassType () cn) cte of
                             Left _ -> error "Internal error while getting class type"
                             Right ct -> return ct
    let ct = T.typename2Type cn
    when (null fs && M.member mn ms) $ do
      (psts,rt) <- case runReaderT (T.getMethodType () ct mn) cte of
          Left _ -> error "Internal error while getting method type"
          Right mt -> return mt
      let pst = psts !! 0
      when (null pst && rt == T.TInt) $ do
        maint <- lift $ functionType False FFI.int32Type []
        mainv <- newNamedFunction FFI.ExternalLinkage mn maint []
        defineFunction mainv $ \_ -> do
          runLLVMCodeGen M.empty env $ genMethodBody T.TInt $ 
            ExpStm $ MethodCall ((),T.TInt) mn [] $ New ((),ct) cn []
        return ()
genClass :: T.ClassTypeEnv -> T.ClassDeclT a -> LLVMCodeGenMethod ()
genClass cte (ClassDecl _ cn _ fs ms) = do
  mvs <- mapM (genMethod cte cn) ms
  let mns = map (\(MethodDecl _ _ mn _ _) -> mn) ms
  let fns = foldr (\(FieldDecl _ _ fn) a -> fn:a) [] fs
  
  genConstr cn fns $ zip mns mvs
  return ()

  where genConstr :: ClassName -> [FieldName] -> [(MethodName,Value)] -> LLVMCodeGenMethod ()
        genConstr cn fs mvs = do 
            env@(cie,_) <- ask
            ci <- lift2 $ getClassInfo cie cn
            pci <-lift2 $ getClassInfo cie $ parentName ci
            let kv = constr ci
            let pkv = constr pci
            
            lift $ defineFunction kv $ \params' -> do
              let params = deSymParametersName params'
              let ppslen = (length params) - (length fs)
              let pps = take ppslen params
              let ppvs' = map snd pps
              let this = ppvs' !! 0
              thiscast <- bitCast this (FFI.pointerType (classType pci) 0)
              let ppvs = (thiscast:(drop 1 ppvs'))
              let cps = drop ppslen params
              let hierarchyt = FFI.pointerType (FFI.pointerType FFI.int8Type 0) 0
              
              tmpVal $ call pkv ppvs
              mapM (uncurry $ initField env this cn) cps
              mapM (uncurry $ initMethod env this cn) mvs
              hierarchyv <- runLLVMCodeGen M.empty env (getField this cn ".hierarchy")
              ctname <- runLLVMCodeGen M.empty env (getField this cn ".ctname")
              
              cnv <- addGlobalString cn ".str"
              store cnv ctname
              hv <- genHierarchy cn $ getHierarchy cie cn
              hv' <- bitCast hv hierarchyt
              store hv' hierarchyv
              retVoid
       
              where initField env this cn fn fv = do
                      field <- runLLVMCodeGen M.empty env (getField this cn fn)
                      store fv field
                    
                    initMethod env this cn mn mv = do
                      method <- runLLVMCodeGen M.empty env (getMethod this cn mn)
                      store mv method
                     
                    getHierarchy cie cn = 
                      case M.lookup cn cie of
                        Nothing -> []
                        Just ci -> [cn]++(getHierarchy cie $ parentName ci)

genHierarchy :: String -> [String] -> CodeGenFunction Value
genHierarchy cn h = do
  trace (show h) $ return ()
  modul <- lift $ getModule
  h' <- mapM (flip addGlobalString ".str") h
  g <- lift2 $ withArrayLen h' $ \len ptr -> do
         let stringt = FFI.pointerType FFI.int8Type 0
         let at = FFI.arrayType stringt $ fromIntegral len
         withCString (".hierarchy."++cn) $ \namePtr -> do
           g <- FFI.addGlobal modul at namePtr
           let arr = FFI.constArray stringt ptr $ fromIntegral len
           FFI.setInitializer g arr
           return arr
  --  gep g [0] ".t"
  return g

getMethodClassType :: T.ClassTypeEnv -> ClassName -> MethodName -> LLVMCodeGenMethod ClassInfo
getMethodClassType cte cn mn = do
  (cie,_) <- ask
  case runReaderT (T.getMethodClassType () (T.typename2Type cn) mn) cte of
    Left _ -> error "Error in getting the base classtype of method"
    Right (T.CT cn _ _ _ _ _) -> lift2 $ getClassInfo cie cn

retVarName = genInternalVarSym "ret"
retLabelName = ".l.end"

genMethod :: T.ClassTypeEnv -> ClassName -> T.MethodDeclT a -> LLVMCodeGenMethod Value
genMethod cte cn (MethodDecl (_,rt) _ mn ps body) = do
  env@(cie,_) <- ask
  ci <- lift2 $ getClassInfo cie cn
  mci <- getMethodClassType cte cn mn
  (ptss,rt) <- case runReaderT (T.getMethodType () (T.typename2Type cn) mn) cte of
                 Left _ -> error "Error in getting method type"
                 Right mt -> return mt
  mt <- lift2 $ makeMethodType cie (classType mci) (ptss !! 0) rt
  let pns = (genUserVarSym "this" : map (\(ParameterDecl _ _ vn) -> genUserVarSym vn) ps)

  mv <- lift $ newNamedFunction FFI.ExternalLinkage (genMethodNameSym cn mn) mt pns
  lift $ defineFunction mv $ \params -> do
    let this = params !! 0
    thiscast <- bitCast (snd this) $ FFI.pointerType (classType ci) 0        
    paramsmap' <- foldM (\a (n,v) -> do v' <- allocaFromValue v
                                        store v v'
                                        return $ M.insert (deSymParameterName n) v' a
                       ) M.empty $ drop 1 params
    let paramsmap = M.insert  (deSymParameterName $ fst this) thiscast paramsmap'
    runLLVMCodeGen paramsmap env $ genMethodBody rt body

  return mv

genMethodBody :: T.Type -> T.StatementT a -> LLVMCodeGen ()
genMethodBody rt body = do
  function <- lift2 $ getFunction
  when (rt /= T.TVoid) $ do rt' <- lift4 $ getFunctionReturnType function
                            retv <- lift2 $ alloca rt' retVarName
                            modify $ M.insert retVarName retv

  mbody <- lift2 $ newNamedBasicBlock ".l.mbody"
  lift2 $ br mbody

  retbb <- lift2 $ newNamedBasicBlock retLabelName
  if rt == T.TVoid then do lift2 $ defineBasicBlock retbb
                           lift2 $ retVoid
    else do lift2 $ defineBasicBlock retbb
            retv <- getVarValue retVarName
            rvtmp <- lift2 $ load retv
            lift2 $ ret rvtmp

  lift2 $ defineBasicBlock mbody
  term <- genStatement retbb body
  when (not term) $ lift2 $ br retbb

getClassNameFromType :: T.Type -> IO ClassName
getClassNameFromType (T.TObjId cn) = return cn
getClassNameFromType (T.TRef t) = getClassNameFromType t
getClassNameFromType _ = error "This type dhave not a name"

genExp :: (T.ExpressionT a) -> LLVMCodeGen Value
genExp = genE
  where genE (Void _) = return $ int1 False -- dummy value
        genE (Null (_,t)) = do
          (cie,_) <- ask
          t' <- lift4 $ runReaderT (type2LLVMType t) cie
          return $ FFI.constNull t'
        genE (B _ v) = return $ int1 v
        genE (I _ v) = return $ int32 v
        genE (S _ v) = lift2 $ addGlobalString v ".str"
        genE (Var _ x) = getVarValue x
        genE (DeRef _ e) = do
          v <- genE e
          lift2 $ load v
        genE (New _ cn ps) = do
          (cie,_) <- ask
          vs <- mapM genE ps
          ci <- lift4 $getClassInfo cie cn
          let kv = constr ci
          kpts <- lift4 $ getFunctionType kv >>= getFunctionParamTypes
          this <- lift2 $ tmpVal $ malloc $ classType ci
          pvs <- lift2 $ mapM (uncurry bitCast) $ zip (this:vs) kpts
          lift2 $ tmpVal $ call kv pvs
          return this
        genE (FieldAccess _ fn e) = do
          this <- genE e
          cn <- lift4 $ getClassNameFromType $ T.getExpType e
          getField this cn fn
        genE (MethodCall _ mn ps e) = do
          this <- genE e
          vs <- mapM genE ps
          cn <- lift4 $ getClassNameFromType $ T.getExpType e
          mvptr <- getMethod this cn mn
          mv <- lift2 $ load mvptr
          mpts <- lift4 $ getFunctionType mv >>= getFunctionParamTypes
          pvs <- lift2 $ mapM (uncurry bitCast) $ zip (this:vs) mpts
          lift2 $ tmpVal $ call mv pvs
        genE (StaticMethodCall _ mn ps cn sig) = do
          (_,sci) <- ask
          vs <- mapM genE ps
          mv <- getStaticCallValue sci cn mn sig
          lift2 $ tmpVal $ call mv vs
        genE (Not _ e) = do
          ev <- genE e
          lift2 $ L.not ev
        genE (Negative _ e) = do
          ev <- genE e
          lift2 $ neg ev
        genE (Equality _ op l r) = binOp op l r
        genE (Relational _ op l r) = binOp op l r
        genE (Additive _ op l r) = binOp op l r
        genE (Multiplicative _ op l r) = binOp op l r
        genE (Boolean _ op l r) = 
          case op of
               "&&" -> and l r
               "||" -> or l r
               _  -> error $ "Operation (" ++ op ++ ") not supported"
        genE (Cast _ tn e) = do
          let stringt = FFI.pointerType FFI.int8Type 0
          let hierarchyt = FFI.pointerType stringt 0    
          (_,sci) <- ask
          ev <- genE e
          let t = T.getExpType e
          if T.primitiveType t then return ev
            else do
             checkcastv <- getStaticCallValue sci "System" ".runtime_castcheck" []
             cn <- getClassName t
             ctnv <- getField ev cn ".ctname"
             hv <- getField ev cn ".hierarchy"
             hv' <- lift2 $ load hv
             ctnv' <- lift2 $ load ctnv
             ts <- lift4 $ newCString tn
             tnv <- lift2 $ addGlobalString tn ".str"
             tnv' <- lift2 $ bitCast tnv stringt
             let tv = FFI.constString ts (fromIntegral $ length tn) (fromIntegral 0)
--             tv' <- lift2 $ bitCast tv stringt
--             let tv' = FFI.constNull stringt
             let tv' = tnv'
             let len = FFI.constInt FFI.int32Type (fromIntegral 1) (fromIntegral 0)
             lift2 $ tmpVal $ call checkcastv [ctnv',hv',len]
             return ev
              
              where getClassName (T.TObjId cn) = return cn
                    getClassName _ = error "Not a class"

        genE _ = error "Expression not implemented"
        
        binOp :: Operation -> T.ExpressionT a -> T.ExpressionT a -> LLVMCodeGen Value
        binOp op l r = do
          lv <- genE l
          rv <- genE r
          case M.lookup op opSem of
            Nothing -> error $ "Operation (" ++ op ++ ") not supported"
            Just ops -> lift2 $ tmpVal $ ops lv rv

        opSem :: M.Map Operation (Value -> Value -> String -> CodeGenFunction Value)
        opSem = M.fromList [("==",iCmp IEq),("!=",iCmp INe),
                            ("<",iCmp ISlt),("<=",iCmp ISle),(">",iCmp ISgt),(">=",iCmp ISge),
                            ("+",add),("-",sub),("*",mul),("/",L.div),("%",L.rem)
                           ]
        and :: T.ExpressionT a -> T.ExpressionT a -> LLVMCodeGen Value
        and l r = do
          end <- lift2 $ newNamedBasicBlock ".l.a-end"
          worst <- lift2 $ newNamedBasicBlock ".l.a-worst"

          lv <- genE l
          best <- lift2 $ getCurrentBB
          lift2 $ brCond lv worst end
          
          lift2 $ defineBasicBlock worst
          rv <- genE r
          worst' <- lift2 $ getCurrentBB
          lift2 $ br end
          
          lift2 $ defineBasicBlock end
          lift2 $ tmpVal $ phi [int1 False,rv] [best,worst']

        or :: T.ExpressionT a -> T.ExpressionT a -> LLVMCodeGen Value
        or l r = do
          end <- lift2 $ newNamedBasicBlock ".l.o-end"
          worst <- lift2 $ newNamedBasicBlock ".l.o-worst"

          lv <- genE l
          best <- lift2 $ getCurrentBB
          lift2 $ brCond lv end worst
          
          lift2 $ defineBasicBlock worst
          rv <- genE r
          worst' <- lift2 $ getCurrentBB
          lift2 $ br end
          
          lift2 $ defineBasicBlock end
          lift2 $ tmpVal $ phi [int1 True,rv] [best,worst']

getStaticCallValue sci cn mn sig = do
  case M.lookup cn sci of
    Nothing -> error $ cn ++ " is not in static environment"
    Just smt -> case M.lookup mn smt of
      Nothing -> error $ mn ++ " is not a static method"
      Just sm -> case M.lookup sig sm of
        Nothing -> error "Signature not exist"
        Just mv -> return mv

genStatement :: BasicBlock -> (T.StatementT a) -> LLVMCodeGen Bool
genStatement retbb (Block ss) = foldM (\term s -> if term then return True
                                                          else genStatement retbb s
                                      ) False ss
genStatement _ (NoOp _) = return False
genStatement _ (ExpStm e) = genExp e >> return False
genStatement _ (Declaration (_,t) _ vn) = do
  (cie,_) <- ask
  llvmt <- lift4 $ runReaderT (type2LLVMType t) cie
  vv <- lift2 $ alloca llvmt $ genUserVarSym vn
  modify $ M.insert vn vv
  return False
genStatement _ (Assign (_,t) dste srce) = do
  (cie,_) <- ask
  dscv <- genExp dste
  srcv <- genExp srce
  srcvt <- lift4 $ runReaderT (type2LLVMType t) cie
  srcv' <- lift2 $ bitCast srcv srcvt
  lift2 $ store srcv' dscv
  return False
genStatement retbb (If _ ce st se) = do
  t <- lift2 $ newNamedBasicBlock ".l.if-then"
  e <- lift2 $ newNamedBasicBlock ".l.if-else"
  merge <- lift2 $ newNamedBasicBlock ".l.if-merge"

  c <- genExp ce
  lift2 $ brCond c t e

  lift2 $ defineBasicBlock t
  tterm <- genStatement retbb st
  when (not tterm) $ lift2 $ br merge
  
  lift2 $ defineBasicBlock e
  eterm <- genStatement retbb se
  when (not eterm) $ lift2 $ br merge

  let term = tterm && eterm
  if term 
    then lift4 $ FFI.deleteBasicBlock merge
    else lift2 $ defineBasicBlock merge

  return term
genStatement retbb (While _ ce s) = do
  cond <- lift2 $ newNamedBasicBlock ".l.wh-cond"
  body <- lift2 $ newNamedBasicBlock ".l.wh-body"
  end <- lift2 $ newNamedBasicBlock ".l.wh-end"
  
  lift2 $ br cond

  lift2 $ defineBasicBlock cond
  c <- genExp ce
  lift2 $ brCond c body end  
  
  lift2 $ defineBasicBlock body  
  term <- genStatement retbb s
  when (not term) $ lift2 $ br cond
  
  if term 
    then lift4 $ FFI.deleteBasicBlock end
    else lift2 $ defineBasicBlock end

  return term 
genStatement retbb (Return (_,rt) e) = do 
  r <- genExp e
  when (rt /= T.TVoid) $ do retv <- getVarValue retVarName
                            lift2 $ store r retv
  lift2 $ br retbb 
  return True

