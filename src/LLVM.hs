module LLVM where

import Prelude hiding (and)

import Data.List
import Data.Int
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader

import Foreign.C.String (withCString, withCStringLen, CString, peekCString,newCString)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray,newArray)
import Foreign.Marshal.Utils (fromBool)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitWriter as FFI

import Debug.Trace

type Value = FFI.ValueRef
type Type = FFI.TypeRef
type Module = FFI.ModuleRef
type Function = Value
type Builder = FFI.BuilderRef
type BasicBlock = FFI.BasicBlockRef

createModule :: String -> IO Module
createModule name =
    withCString name $ \ namePtr -> FFI.moduleCreateWithName namePtr

-- ~ From LLVM.Core.Util
functionType :: Bool -> Type -> [Type] -> IO Type
functionType varargs retType paramTypes =
    withArrayLen paramTypes $ \ len ptr ->
        return $ FFI.functionType retType ptr (fromIntegral len)
	       	 		  (fromBool varargs)

structCreateNamed :: String -> IO Type
structCreateNamed name = do
  withCString name $ \namePtr -> do
    context <- FFI.getGlobalContext
    FFI.structCreateNamed context namePtr

structSetBody :: Type -> [Type] -> Bool -> IO ()
structSetBody struct types packed = do
    withArrayLen types $ \len typesPtr -> do 
      FFI.structSetBody struct typesPtr (fromIntegral len) (if packed then 1 else 0)

structTypeNamed :: String -> [Type] -> Bool -> IO Type
structTypeNamed name types packed = do
  struct <- structCreateNamed name
  structSetBody struct types packed
  return struct

-- ~ From LLVM.Core.Util
writeBitcodeToFile :: String -> Module -> IO ()
writeBitcodeToFile name mdl =
    withCString name $ \ namePtr -> do
      rc <- FFI.writeBitcodeToFile mdl namePtr
      when (rc /= 0) $
        ioError $ userError $ "writeBitcodeToFile: return code " ++ show rc
      return ()

-- From LLVM.Core.Util
getValueNameU :: Value -> IO String
getValueNameU a = do
    -- sometimes void values need explicit names too
    cs <- FFI.getValueName a
    str <- peekCString cs
    if str == "" then return (show a) else return str

-- From LLVM.Core.Util
annotateValueList :: [Value] -> IO [(String, Value)]
annotateValueList vs = do
  names <- mapM getValueNameU vs
  return $ zip names vs

{--
deepTypeKindEq :: Value -> Value -> IO Bool
deepTypeKindEq v1 v2 = do
  v1t <- FFI.typeOf v1
  v2t <- FFI.typeOf v2
  eq v1 v2

 where eq :: Type -> Type -> IO Bool
       eq t1 t2 = do
         p1k <- FFI.getTypeKind p1
         p2k <- FFI.getTypeKind p1
         
         if p1k != p2k then return False
           else  
             case p1k of
               FFI.VoidTypeKind -> return True
               FFI.FloatTypeKind -> return True
               FFI.DoubleTypeKind -> return True
               FFI.X86_FP80TypeKind -> return True
               FFI.FP128TypeKind -> return True
               FFI.PPC_FP128TypeKind -> return True
               FFI.LabelTypeKind -> return True
               FFI.OpaqueTypeKind -> return False
               FFI.IntegerTypeKind -> do w1 <- FFI.getIntTypeWidth p1
                                         w2 <- FFI.getIntTypeWidth p2
                                         if w1 == w2 then return True
                                                     else return False
               FFI.PointerTypeKind -> do t1 <- FFI.getElementType p1 
                                         t2 <- FFI.getElementType p2
                                         eq t1 t2
               FFI.ArrayTypeKind -> do n1 <- FFI.getArrayLength p1
                                       n2 <- FFI.getArrayLength p2
                                       if n1 != n2 then do
                                           t1 <- FFI.getElementType p1
                                           t2 <- FFI.getElementType p2
                                           eq t1 t2
                                         else return False
               FFI.VectorTypeKind ->do n1 <- FFI.getVectorSize p1
                                       n2 <- FFI.getVectorSize p2
                                       if n1 != n2 then do
                                           t1 <- FFI.getElementType p1
                                           t2 <- FFI.getElementType p2
                                           eq t1 t2
                                         else return False
               FFI.FunctionTypeKind -> do
                 c1 <- FFI.countParamTypes p1
                 c2 <- FFI.countParamTypes p2
                 if c1 == c2 then do
                   rt1 <- FFI.getReturnType p1
                   rt2 <- FFI.getReturnType p1
                   
                   rt <- eq rt1 rt2
                   if rt then do
                     let n1 = fromIntegral c1
                     let n2 = fromIntegral c2
                   
                     as1 <- allocaArray n1 $ \ args -> do
                            FFI.getParamTypes p1 args
                            peekArray n1 args
                     as2 <- allocaArray n2 $ \ args -> do
                            FFI.getParamTypes p2 args
                            peekArray n2 args
                     
                     foldM (\(r c) -> if r then (uncurry eq) c
                                           else return False     
                           ) zip as1 as2
                     else return False
                   else return False

               FFI.StructTypeKind -> do
                 c1 <- FFI.countStructElementTypes p1
                 c2 <- FFI.countStructElementTypes p2
                 if c1 == c2 then do
                     let n1 = fromIntegral c1
                     let n2 = fromIntegral c2
                   
                     as1 <- allocaArray n1 $ \ args -> do
                            FFI. p1 args
                            peekArray n1 args
                     as2 <- allocaArray n2 $ \ args -> do
                            FFI.getParamTypes p2 args
                            peekArray n2 args
                     
                     foldM (\(r c) -> if r then (uncurry eq) c
                                           else return False     
                           ) zip as1 as2
                   else return False
--}

type GlobalString = M.Map String FFI.ValueRef
data CGMState =  CGMState { cgm_module :: Module, cgm_globalstring :: GlobalString }
type CodeGenModule = StateT CGMState IO
data CGFState =  CGFState { cgf_builder :: Builder, cgf_function :: Function }
type CodeGenFunction = StateT CGFState CodeGenModule

runCodeGenFunction :: Builder -> Function -> CodeGenFunction a -> CodeGenModule a
runCodeGenFunction builder fun body = do
    let cgf = CGFState { cgf_builder = builder,
    	      	       	 cgf_function = fun }
    evalStateT body cgf

runCodeGenModule :: Module -> CodeGenModule a -> IO a
runCodeGenModule modul body = do
    let cgm = CGMState { cgm_module = modul, cgm_globalstring = M.empty }
    evalStateT body cgm

lift2 = lift . lift

getModule :: CodeGenModule Module
getModule = gets cgm_module

getGlobalStringMap :: CodeGenModule GlobalString
getGlobalStringMap = gets cgm_globalstring

setGlobalStringMap :: GlobalString -> CodeGenModule ()
setGlobalStringMap gs = modify $ \s -> s{cgm_globalstring=gs}

getBuilder :: CodeGenFunction Builder
getBuilder = gets cgf_builder

getFunction :: CodeGenFunction Function
getFunction = gets cgf_function

addGlobalString :: String -> String -> CodeGenFunction Value
addGlobalString strname strvalue = do
  gsm <- lift $ getGlobalStringMap
  case M.lookup strvalue gsm of
    Just v -> return v
    Nothing -> do builder <- getBuilder
                  strnamePtr <- lift2  $ newCString strname
                  strvaluePtr <- lift2 $ newCString strvalue
                  v <- lift2 $ FFI.buildGlobalString builder strnamePtr strvaluePtr
                  lift $ setGlobalStringMap $ M.insert strvalue v gsm
                  return v

constInt :: Type -> Int -> Value
constInt t v = do
  FFI.constInt t (fromIntegral v) (fromIntegral 1)

getFunctionType :: Function -> IO Type
getFunctionType fun = do 
  ft <- FFI.typeOf fun
  ftk <- FFI.getTypeKind ft
  case ftk of
    FFI.FunctionTypeKind -> return ft
    FFI.PointerTypeKind -> FFI.getElementType ft >>= return
    k -> error $ "Not a function (" ++ (show k) ++ ")"

getFunctionReturnType :: CodeGenFunction Type
getFunctionReturnType = do
  function <- getFunction
  ft <- lift2 $ getFunctionType function
  lift2 $ FFI.getReturnType ft

newNamedFunction :: FFI.Linkage -> String -> Type -> [String] -> CodeGenModule Function
newNamedFunction linkage name funtype paramnames = do
  modul <- getModule
  lift $ withCString name $ \ namePtr -> do
    f <- FFI.addFunction modul namePtr funtype
    FFI.setLinkage f (FFI.fromLinkage linkage)
    setFunctionParamNames f paramnames
    return f

defineFunction :: Function -> ([(String,Value)] -> CodeGenFunction ()) -> CodeGenModule ()
defineFunction fun body = do
  builder <- lift $ FFI.createBuilder
  let pn = fromIntegral $ FFI.countParams fun
  paramvalues <- lift $ allocaArray pn $ \ args -> do
                           FFI.getParams fun args
		           peekArray pn args
  paramtuple <- lift $ annotateValueList paramvalues
  let body' = newNamedBasicBlock "" >>= defineBasicBlock >> (body paramtuple)
  runCodeGenFunction builder fun body'

getFunctionParam :: Function -> Int -> Value
getFunctionParam fun = do
  FFI.getParam fun . fromIntegral

getParam :: Int -> CodeGenFunction Value
getParam nth = do
  function <- getFunction
  return $ getFunctionParam function nth

setFunctionParamNames :: Function -> [String] -> IO ()
setFunctionParamNames fun paramnames = 
  mapM_ (\(name,pos) -> withCString name $ \namePtr ->
          FFI.setValueName (getFunctionParam fun pos) namePtr
        ) $ zip paramnames [0..]

newNamedBasicBlock :: String -> CodeGenFunction BasicBlock
newNamedBasicBlock name = do
  function <- getFunction
  lift2 $ withCString name $ \ namePtr -> FFI.appendBasicBlock function namePtr

defineBasicBlock :: BasicBlock -> CodeGenFunction ()
defineBasicBlock bb = do
  builder <- getBuilder
  lift2 $ FFI.positionAtEnd builder bb

abinop :: (Builder -> Value -> Value -> CString -> IO Value)
       -> String -> Value -> Value -> CodeGenFunction Value
abinop op id vl vr = do
  builder <- getBuilder
  lift2 $ withCString id $ \idPtr -> op builder vl vr idPtr

add = abinop FFI.buildAdd
  
store :: Value -> Value -> CodeGenFunction Value
store src dst = do
  builder <- getBuilder
  lift2 $ FFI.buildStore builder src dst

structGEP :: Value -> Int -> String -> CodeGenFunction Value
structGEP struct offset name = do
  builder <- getBuilder
  lift2 $ withCString name $ \namePtr ->
    FFI.buildStructGEP builder struct (fromIntegral offset) namePtr

gep :: Value -> [Int] -> String -> CodeGenFunction Value
gep this indexs name = do
  builder <- getBuilder
  let i32indexs = map (constInt FFI.int32Type) indexs
  lift2 $ withArrayLen i32indexs $ \ len ptr -> 
         withCString name $ \namePtr ->
           FFI.buildInBoundsGEP builder this ptr (fromIntegral len) namePtr

call :: Value -> [Value] -> String -> CodeGenFunction Value
call fun params name = do
  builder <- getBuilder
  paramsPtr <- lift2 $ newArray params
  let len = fromIntegral $ length params
  rt <- getFunctionReturnType
  rtk <- lift2 $ FFI.getTypeKind rt
  name' <- case rtk of
             FFI.VoidTypeKind -> return ""
             _ -> return name
  lift2 $ withCString name' $ \namePtr ->
    FFI.buildCall builder fun paramsPtr len namePtr

bitCast :: Value -> Type -> CodeGenFunction Value
bitCast value t = do
  builder <- getBuilder
  namePtr <- lift2 $ FFI.getValueName value
  name <- lift2 $ peekCString namePtr
  lift2 $ withCString name $ \namePtr ->
    FFI.buildBitCast builder value t namePtr

malloc :: Type -> String -> CodeGenFunction Value
malloc t name = do
  builder <- getBuilder
  lift2 $ withCString name $ \namePtr ->
    FFI.buildMalloc builder t namePtr

ret :: Value -> CodeGenFunction ()
ret v = do 
  builder <- getBuilder
  lift2 $ FFI.buildRet builder v
  return ()

retVoid :: CodeGenFunction ()
retVoid = do 
  builder <- getBuilder
  lift2 $ FFI.buildRetVoid builder
  return ()
