module ToLLVM where

import Ast
import qualified Typesystem as T
import LLVM
import qualified LLVM.FFI.Core as FFI

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

genLLVMCode :: T.ProgramT a -> T.ClassTypeEnv -> FilePath -> IO ()
genLLVMCode p cte output = do
  modul <- createModule output
  runCodeGenModule modul $ buildClassInfoEnv cte
  writeBitcodeToFile output modul

type FieldOffSets = [(FieldName,Int)]
type MethodOffSets = [(MethodName,Int)]
type MethodTable = M.Map MethodName Value
data ClassInfo = ClassInfo { classType :: Type,
                             fieldOffsets :: FieldOffSets,
                             methodOffsets :: MethodOffSets,
                             methodValues :: MethodTable
                           } deriving Show

builtInClassInfoObject :: IO ClassInfo
builtInClassInfoObject = do structbodyt <- structTypeNamed "Object" [FFI.pointerType FFI.int8Type 0] False
                            let filedoffset = [(".vtable",0)]
                            return $ ClassInfo structbodyt filedoffset [] M.empty
                           
builtInClassInfoMap :: M.Map ClassName (IO ClassInfo)
builtInClassInfoMap = M.insert "Object" builtInClassInfoObject M.empty

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

genClassNameSym :: String -> String
genClassNameSym = (".struct." ++)

genMethodNameSym :: String -> String -> String
genMethodNameSym cn = ((".method." ++ cn) ++) . ("." ++)


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
                     case M.lookup cn builtInClassInfoMap of
                       Just cic -> do ci <- lift $ lift $ lift $ cic
                                      let cie' = M.insert cn ci cie
                                      let visited' = S.insert cn visited
                                      put (cie',visited')
                       Nothing -> do
                         when (not $ S.member pn visited) $ buildCIE pn -- assert not $ null pn
                         case M.lookup pn cie of
                           Nothing -> error "Parent info must be in the environment"
                           Just pi -> do
                             case M.lookup cn cie of
                               Nothing -> error $ "Malformed cie. Cannot find " ++ cn ++ " in cie.\n" ++ (show cie)
                               Just ci -> do let ct = classType ci
                                             let pt = classType pi
                                             structbody <- lift $ lift $ lift $ runReaderT (buildStructBody ct fns ftm mtm) cie
                                             let (fos,mos) = makeOffsets (getLastOffset pi) fns (M.keys mtm)
                                             lift $ lift $ lift $ structSetBody ct (pt:structbody) False
                                             mt <- initMethods cn ct mtm
                                             let ci' = ClassInfo (ct) fos mos mt
                                             let cie' = M.insert cn ci' cie
                                             let visited' = S.insert cn visited
                                             put (cie',visited')

       buildBaseCIE :: T.ClassTypeEnv -> IO ClassInfoEnv
       buildBaseCIE cte = liftM  M.fromList (mapM makePair $ M.keys cte)

       makePair :: ClassName -> IO (ClassName,ClassInfo)
       makePair k = do t <- structCreateNamed $ genClassNameSym k
                       return $ (k,ClassInfo t [] [] M.empty)

       getLastOffset :: ClassInfo -> Int
       getLastOffset ci = let fo = map snd $ fieldOffsets ci
                              mo = map snd $ methodOffsets ci
                              offsets = fo ++ mo in
                          if null offsets then 0 else maximum offsets

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
                     ) $ sort $ M.keys mtm
         return $ sft ++ smt

       makeOffsets :: Int -> [FieldName] -> [MethodName] -> (FieldOffSets,MethodOffSets)
       makeOffsets boffset fns mns = (fos,mos)
         where fosmin = boffset + 1
               fosmax = fosmin + length fns
               mosmin = fosmax + 1
               mosmax = mosmin + length mns
               fos = zip fns [fosmin..fosmax]
               mos = zip mns [mosmin..mosmax]

       initMethods :: ClassName -> Type -> T.MethodsType -> BuildClassInfoEnvComp MethodTable
       initMethods cn ct mtm = foldM (initMethod ct) M.empty $ M.toList mtm
         where initMethod :: Type -> MethodTable -> (MethodName,([[T.Type]],T.Type)) -> BuildClassInfoEnvComp MethodTable
               initMethod ct mtm (mn,(ptss,rt)) = do
                 (cie,_) <- get
                 mt <- lift $ lift $ lift $ runReaderT (makeMethodType ct (ptss !! 0) rt) cie
                 mv <- lift $ lift $ newNamedFunction FFI.InternalLinkage (genMethodNameSym cn mn) mt []
                 return $ M.insert mn mv mtm
