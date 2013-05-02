module LLVM where

import Prelude hiding (and)

import Data.Int
import Data.Map as M

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader

import Foreign.C.String (withCString, withCStringLen, CString, peekCString,newCString)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray)
import Foreign.Marshal.Utils (fromBool)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitWriter as FFI

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


type GlobalString = Map String FFI.ValueRef
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

getFunctionType :: Function -> IO Type
getFunctionType = FFI.typeOf

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
  {--ft <- lift $ getFunctionType fun
  pnPtr <- FFI.countParamTypes ft
  let pn = fromIntegral pnPtr
  paramType <- allocaArray n $ \ args -> do
		     FFI.getParamTypes p args
		     peekArray n args --}
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
  
ret :: Value -> CodeGenFunction ()
ret v = do 
  builder <- getBuilder
  lift2 $ FFI.buildRet builder v
  return ()

