module ToLLVM where

import Ast
import qualified Typesystem as T
import LLVM hiding (lift2)
--import qualified LLVM as L
import qualified LLVM.FFI.Core as FFI

import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Foreign.Marshal.Array (withArrayLen)
import Foreign.C.String (peekCString)

import Debug.Trace

--lift2 = lift . lift

genLLVMCode :: T.ProgramT a -> T.ClassTypeEnv -> FilePath -> IO ()
genLLVMCode p cte output = do
  modul <- createModule output
  cie <- runCodeGenModule modul $ buildClassInfoEnv cte
  writeBitcodeToFile output modul

type FieldOffSets = M.Map FieldName Int
type MethodOffSets = M.Map MethodName Int
type MethodsTable = M.Map MethodName Value
data ClassInfo = ClassInfo { classType :: Type,
                             parentName :: ClassName,
                             fieldOffsets :: FieldOffSets,
                             methodOffsets :: MethodOffSets,
                             methodsTable :: MethodsTable
                           } deriving Show

builtInClassInfoObject :: Type -> CodeGenModule ClassInfo
builtInClassInfoObject ct = do
  lift $ structSetBody ct [FFI.int8Type] False
  kv <- makeConstr ct
  let methodtable = M.insert (genConstrSym "") kv M.empty
  return $ ClassInfo ct "" filedoffset methodoffset methodtable

  where filedoffset = M.fromList $ [(".dummy",0)]
        methodoffset = M.empty
        makeConstr ct = do kt <- lift $ functionType False FFI.voidType [FFI.pointerType ct 0]
                           kv <- newNamedFunction FFI.ExternalLinkage (genConstrSym "Object") kt []
                           defineFunction kv $ \_ -> retVoid
                           return kv

builtInClassInfoMap :: M.Map ClassName (Type -> CodeGenModule ClassInfo)
builtInClassInfoMap = M.insert "Object" (builtInClassInfoObject) M.empty

type ClassInfoEnv = (M.Map ClassName ClassInfo)
type BuildClassInfoEnvComp = ReaderT T.ClassTypeEnv (StateT (ClassInfoEnv,S.Set ClassName) CodeGenModule)
type ClassInfoEnvComp = ReaderT (ClassInfoEnv) IO

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
genUserVarSym = (".u." ++)

genInternalVarSym :: String -> String
genInternalVarSym = (".i." ++)

tmpVal :: (String -> a) -> a
tmpVal = ($ ".t")

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
                           Just cic -> do ci <- lift $ lift $ cic $ classType ci
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
                                             let pmst = methodsTable pi
                                             structbody <- lift $ lift $ lift $ runReaderT (buildStructBody ct fns ftm (mtm `M.difference` pmst)) cie
                                             let (fos,mos) = makeOffsets (getLastOffset pi) fns (M.keys mtm \\ M.keys pmst)
                                             lift $ lift $ lift $ structSetBody ct (pt:structbody) False
                                             mt <- initMethods cn ct mtm

                                             let ci = ClassInfo (ct) pn fos mos mt
                                             let cie' = M.insert cn ci cie
                                             let visited' = S.insert cn visited
                                             put (cie',visited')
                                             makeConstr cn ct
                                             return ()

       buildBaseCIE :: T.ClassTypeEnv -> IO ClassInfoEnv
       buildBaseCIE cte = liftM  M.fromList (mapM makePair $ M.keys cte)

       makePair :: ClassName -> IO (ClassName,ClassInfo)
       makePair k = do t <- structCreateNamed $ genClassNameSym k
                       return $ (k,ClassInfo t "" M.empty M.empty M.empty)

       
       getLastOffset :: ClassInfo -> Int
       getLastOffset ci = 0
       {--
       getLastOffset ci = let fo = M.elems $ fieldOffsets ci
                              mo = M.elems $ methodOffsets ci
                              offsets = fo ++ mo in
                          if null offsets then 0 else maximum offsets
       --}

       makeMethodType :: Type -> [T.Type] -> T.Type -> ClassInfoEnvComp Type
       makeMethodType ct pts rt = do
         rt' <- type2LLVMType rt
         pts' <- mapM type2LLVMType $ pts
         let ct' = FFI.pointerType (ct) 0
         lift $ functionType False rt' (ct':pts')

       buildStructBody :: Type -> [FieldName] -> T.FieldsType -> T.MethodsType -> ClassInfoEnvComp [Type]
       buildStructBody ct fns ftm mtm = do 
         sft <- mapM (\fn -> case M.lookup fn ftm of
                               Nothing -> error "FieldName must be in the map"
                               Just t -> type2LLVMType t
                     ) fns
         smt <- mapM (\mn -> case M.lookup mn mtm of
                               Nothing -> error "MethodName must be in the map"
                               Just (ptss,rt) -> do -- We handle only the first signature
                                                    -- Multiple signature method should be only in static class
                                                    mt <- makeMethodType ct (ptss !! 0) rt
                                                    let mt' = FFI.pointerType mt 0
                                                    return mt'
                     ) $ M.keys mtm
         return $ sft ++ smt

       makeOffsets :: Int -> [FieldName] -> [MethodName] -> (FieldOffSets,MethodOffSets)
       makeOffsets boffset fns mns = (fos,mos)
         where fosmin = boffset + 1
               fosmax = fosmin + length fns
               mosmax = fosmax + length mns
               fos = M.fromList $ zip fns [fosmin..fosmax]
               mos = M.fromList $ zip mns [fosmax..mosmax]

       initMethods :: ClassName -> Type -> T.MethodsType -> BuildClassInfoEnvComp MethodsTable
       initMethods cn ct mtm = foldM (initMethod ct) M.empty $ M.toList mtm
         where initMethod :: Type -> MethodsTable -> (MethodName,([[T.Type]],T.Type)) -> BuildClassInfoEnvComp MethodsTable
               initMethod ct mtm (mn,(ptss,rt)) = do
                 (cie,_) <- get
                 mt <- lift $ lift $ lift $ runReaderT (makeMethodType ct (ptss !! 0) rt) cie
                 mv <- lift $ lift $ newNamedFunction FFI.ExternalLinkage (genMethodNameSym cn mn) mt []
                 return $ M.insert mn mv mtm

       makeConstr :: ClassName -> Type -> BuildClassInfoEnvComp Value
       makeConstr cn ct  =
         do (cie,visited) <- get
            cte <- ask
            fns <- case runReaderT (T.getContrFields () cn) cte of
                     Left _ -> error "Internal error while getting the constructr parameters"
                     Right fns -> return fns
            fts <- case runReaderT (T.getContructorType () cn) cte of
                     Left _ -> error "Internal error while getting the constructr parameters type"
                     Right fts -> return fts
            (T.CT _ pn fs _ _ _) <- case runReaderT (T.getClassType () cn) cte of
                     Left _ -> error "Internal error while getting class type"
                     Right ct -> return ct
            ci <- case M.lookup cn cie of
                    Nothing -> error "Internal error while getting class info" 
                    Just ci -> return ci
            pi <- case M.lookup pn cie of
                    Nothing -> error "Internal error while getting parent class info" 
                    Just pi -> return pi
            let pt = classType pi
            pkv <- case M.lookup (genConstrSym "") $ methodsTable pi of
                     Nothing -> error "Internal error while getting parent constructor"
                     Just pkv -> return pkv

            kt <- lift $ lift $ lift $ runReaderT (makeMethodType ct fts T.TVoid) cie
            kv <- lift $ lift $ newNamedFunction FFI.ExternalLinkage (genConstrSym cn) kt $ map genUserVarSym ("this":fns)
            
            let ci' = ci{methodsTable = M.insert (genConstrSym "") kv $ methodsTable ci}
            let cie'= M.insert cn ci' cie
            put(cie',visited)

            lift $ lift $ defineFunction kv $ \params' -> do

              let params = map (\(n,v) -> (drop (length $ genUserVarSym "") n,v)) params'
              let ppslen = (length params) - (length fs)
              let pps = take ppslen params
              let ppvs' = map snd pps
              let this = ppvs' !! 0
              thiscast <- bitCast this (FFI.pointerType pt 0)
              let ppvs = (thiscast:(drop 1 ppvs'))

              let cps = drop ppslen params
              
              {--
              trace ("cie:" ++ (show cie)) $ return ()
              trace ("START " ++ cn) $ return ()
              trace ("params:" ++ (show params)) $ return ()
              trace ("ppslen:" ++ (show ppslen)) $ return ()
              trace ("pps:" ++ (show pps)) $ return ()
              trace ("cps:" ++ (show cps)) $ return ()

              trace ("callign parent construcotr") $ return ()       

              trace "call parent constructor" $ return ()
              
              trace "initialized fields" $ return ()
              --}
                  
              tmpVal $ call pkv ppvs
              mapM (uncurry $ initField cie' this cn) cps
              mapM (uncurry $ initMethod cie' this cn) $ M.toList $ methodsTable ci
              retVoid
              --trace ("FINISH " ++ cn) $ return ()
            return kv
       
              where --initField :: Value -> FieldName -> Value -> CodeGenFunction Value
                    initField cie this cn fn fv = do 
                      field <- runReaderT (getField this cn fn) cie
                      store fv field
                    
                    initMethod cie this cn mn mv = do 
                      method <- runReaderT (getMethod this cn mn) cie
--                      mvt <- lift $ lift $ FFI.typeOf mv
--                      method' <- bitCast method mvt
                      store mv method

type GenCodeExp = ReaderT ClassInfoEnv CodeGenFunction

lift3 = lift . lift . lift

getClassInfo :: ClassName -> GenCodeExp ClassInfo
getClassInfo cn = do
  cie <- ask
  case M.lookup cn cie of
    Nothing -> error "Class is not in environment"
    Just ci -> return ci  

getMethodValue :: ClassName -> MethodName -> GenCodeExp Value
getMethodValue cn mn = do
  ci <- getClassInfo cn
  cie <- ask
  case M.lookup mn $ methodsTable ci of
    Nothing -> error "Method is not in the table"
    Just mv -> return mv

getField = getMember fieldOffsets
getMethod this cn mn = do 
  method <- getMember methodOffsets this cn mn
  mv <- getMethodValue cn mn
  mvt <- lift3 $ FFI.typeOf mv
  let mvt' = FFI.pointerType mvt 0
  lift $ bitCast method mvt'

getMember :: (ClassInfo -> M.Map String Int) -> Value -> ClassName -> String -> GenCodeExp Value
getMember getter this cn mn = do
  indexs <- getMemberIndex getter cn mn [0]
  lift $ tmpVal $ gep this indexs

  where getMemberIndex :: (ClassInfo -> M.Map String Int) -> ClassName -> String -> [Int] -> GenCodeExp [Int]
        getMemberIndex getter cn mn index = do
          when (null cn) $ error "Member not found"
          cie <- ask
          ci <- case M.lookup cn cie of
            Nothing -> error "Class is not in environment"
            Just ci -> return ci
  
          case M.lookup mn $ getter ci of
            Nothing -> getMemberIndex getter (parentName ci) mn $ index ++ [0] -- parent is always the first
            Just offset -> return $ index ++ [offset]
