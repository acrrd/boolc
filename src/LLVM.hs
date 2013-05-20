module LLVM where

import Prelude hiding (and,not)

import Data.Char (isDigit)
import Data.List
import Data.Int
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader

import Foreign.Ptr
import Foreign.C.String (withCString, withCStringLen, CString, peekCString,newCString)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray,newArray)
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

type GlobalString = M.Map String FFI.ValueRef
data CGMState =  CGMState { cgm_module :: Module, cgm_globalstring :: GlobalString }
type CodeGenModule = StateT CGMState IO
data CGFState =  CGFState { cgf_builder :: Builder, cgf_function :: Function, cgf_currentbb :: BasicBlock }
type CodeGenFunction = StateT CGFState CodeGenModule

runCodeGenFunction :: Builder -> Function -> CodeGenFunction a -> CodeGenModule a
runCodeGenFunction builder fun body = do
    let cgf = CGFState { cgf_builder = builder,
    	      	       	 cgf_function = fun,
                         cgf_currentbb = nullPtr
                       }
    evalStateT body cgf

runCodeGenModule :: Module -> CodeGenModule a -> IO a
runCodeGenModule modul body = do
    let cgm = CGMState { cgm_module = modul, cgm_globalstring = M.empty }
    evalStateT body cgm

lift2 f = (lift . lift) f

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

getCurrentBB :: CodeGenFunction BasicBlock
getCurrentBB = gets cgf_currentbb

setCurrentBB :: BasicBlock -> CodeGenFunction ()
setCurrentBB bb = modify $ \s -> s{cgf_currentbb=bb}

addGlobalString :: String -> String -> CodeGenFunction Value
addGlobalString strvalue strname = do
  gsm <- lift $ getGlobalStringMap
  case M.lookup strvalue gsm of
    Just v -> return v
    Nothing -> do builder <- getBuilder
                  strnamePtr <- lift2  $ newCString strname
                  strvaluePtr <- lift2 $ newCString strvalue
                  strv <- lift2 $ FFI.buildGlobalString builder strvaluePtr strnamePtr
                  v <- gep strv [0,0] ""
                  lift $ setGlobalStringMap $ M.insert strvalue v gsm
                  return v

getValueNameRaw :: Value -> CodeGenFunction String
getValueNameRaw v = lift2 $ (FFI.getValueName v >>= peekCString)

getValueName :: Value -> CodeGenFunction String
getValueName v = do name <- getValueNameRaw v
                    return $ reverse $ dropWhile isDigit $ reverse name

getValueNamePtr :: Value -> CodeGenFunction CString
getValueNamePtr v = do
  name <- getValueName v
  lift2 $ withCString name $ \namePtr -> return namePtr

constInt :: Type -> Int -> Value
constInt t v = do
  FFI.constInt t (fromIntegral v) (fromIntegral 1)

getFunctionType :: Function -> IO Type
getFunctionType fun = FFI.typeOf fun >>= getFT
  where getFT ft = do
          ftk <- FFI.getTypeKind ft
          case ftk of
            FFI.FunctionTypeKind -> return ft
            FFI.PointerTypeKind -> FFI.getElementType ft >>= getFT
            k -> error $ "Not a function (" ++ (show k) ++ ")"

getFunctionReturnType :: Function -> IO Type
getFunctionReturnType function = do
  ft <- getFunctionType function
  FFI.getReturnType ft

newNamedFunction :: FFI.Linkage -> String -> Type -> [String] -> CodeGenModule Function
newNamedFunction linkage name funtype paramnames = do
  modul <- getModule
  lift $ withCString name $ \ namePtr -> do
    f <- FFI.addFunction modul namePtr funtype
    FFI.setLinkage f (FFI.fromLinkage linkage)
    setFunctionParamNames f paramnames
    return f

defineFunction :: Function -> ([(String,Value)] -> CodeGenFunction a) -> CodeGenModule ()
defineFunction fun body = do
  builder <- lift $ FFI.createBuilder
  let pn = fromIntegral $ FFI.countParams fun
  paramvalues <- lift $ allocaArray pn $ \ args -> do
                          FFI.getParams fun args
                          peekArray pn args
  paramtuple <- lift $ annotateValueList paramvalues
  let body' = newNamedBasicBlock "" >>= defineBasicBlock >> (body paramtuple)
  runCodeGenFunction builder fun body'
  return ()

getFunctionParam :: Function -> Int -> Value
getFunctionParam fun = do
  FFI.getParam fun . fromIntegral

getFunctionParamTypes :: Type -> IO [Type]
getFunctionParamTypes ft = do
  pn <- FFI.countParamTypes ft
  let n = fromIntegral pn
  allocaArray n $ \args -> do
    FFI.getParamTypes ft args
    peekArray n args

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
  setCurrentBB bb

lastIsTerminating :: CodeGenFunction Bool
lastIsTerminating = do
  bb <- getCurrentBB
  instr <- lift2 $ FFI.getLastInstruction bb
  if instr == nullPtr then return False
    else do
      opcode <- lift2 $ FFI.instGetOpcode instr
      if any (==opcode) [1..5] -- http://llvm.org/docs/doxygen/html/Core_8h_source.html#l00172
        then return True
        else return False                
  
abinop :: (Builder -> Value -> Value -> CString -> IO Value)
       -> Value -> Value -> String -> CodeGenFunction Value
abinop op vl vr id = do
  builder <- getBuilder
  lift2 $ withCString id $ \idPtr -> op builder vl vr idPtr

add = abinop FFI.buildAdd
sub = abinop FFI.buildSub
and = abinop FFI.buildAnd
mul = abinop FFI.buildMul
div = abinop FFI.buildSDiv
rem = abinop FFI.buildSRem

not :: Value -> CodeGenFunction Value
not v = do
  builder <- getBuilder
  namePtr <- getValueNamePtr v
  lift2 $ FFI.buildNot builder v namePtr
  
neg :: Value -> CodeGenFunction Value
neg v = do
  builder <- getBuilder
  namePtr <- getValueNamePtr v
  lift2 $ FFI.buildNeg builder v namePtr

data CmpOp = IEq | INe | IUgt | IUge | IUlt | IUle | ISgt | ISge | ISlt | ISle

cmpOpCode op = fromIntegral $
  case op of
    IEq  -> 32
    INe  -> 33
    IUgt -> 34
    IUge -> 35
    IUlt -> 36
    IUle -> 37
    ISgt -> 38
    ISge -> 39
    ISlt -> 40
    ISle -> 41

iCmp :: CmpOp -> Value -> Value -> String -> CodeGenFunction Value
iCmp op v1 v2 name = do
  builder <- getBuilder
  lift2 $ withCString name $ \namePtr ->
    FFI.buildICmp builder (cmpOpCode op) v1 v2 namePtr

phi :: [Value] -> [Value] -> String -> CodeGenFunction Value
phi vs bs name = do
  when (length vs /= length bs) $ error "Values and blocks must have the same arity"
  when (null vs) $ error "Phi must have at least one value and one block"
  
  builder <- getBuilder
  t <- lift2 $ FFI.typeOf $ vs !! 0
  p <- lift2 $ withCString name $ \namePtr ->
         FFI.buildPhi builder t namePtr
  vsptr <- lift2 $ newArray vs
  bsptr <- lift2 $ newArray bs
  let len = fromIntegral $ length vs
  lift2 $ FFI.addIncoming p vsptr bsptr len
  return p

select c v1 v2 name = do
  builder <- getBuilder
  lift2 $ withCString name $ \namePtr ->
    FFI.buildSelect builder c v1 v2 namePtr

store :: Value -> Value -> CodeGenFunction ()
store src dst = do
  builder <- getBuilder
  lift2 $ FFI.buildStore builder src dst
  return ()

load :: Value -> CodeGenFunction Value
load src = do
  builder <- getBuilder
  namePtr <- getValueNamePtr src
  lift2 $ FFI.buildLoad builder src namePtr

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
  rt <- lift2 $ getFunctionReturnType fun
  rtk <- lift2 $ FFI.getTypeKind rt
  let name' = if rtk == FFI.VoidTypeKind then "" else name
  lift2 $ withCString name' $ \namePtr ->
    FFI.buildCall builder fun paramsPtr len namePtr

bitCast :: Value -> Type -> CodeGenFunction Value
bitCast value t = do
  builder <- getBuilder
  namePtr <- getValueNamePtr value
  name <- lift2 $ peekCString namePtr
  lift2 $ withCString name $ \namePtr ->
    FFI.buildBitCast builder value t namePtr

trunc :: Value -> Type -> CodeGenFunction Value
trunc value t = do
  builder <- getBuilder
  namePtr <- getValueNamePtr value
  name <- lift2 $ peekCString namePtr
  lift2 $ withCString name $ \namePtr ->
    FFI.buildTrunc builder value t namePtr

malloc :: Type -> String -> CodeGenFunction Value
malloc t name = do
 builder <- getBuilder
 lift2 $ withCString name $ \namePtr ->
   FFI.buildMalloc builder t namePtr


alloca :: Type -> String -> CodeGenFunction Value
alloca t name = do
  builder <- getBuilder
  lift2 $ withCString name $ \namePtr ->
    FFI.buildAlloca builder t namePtr

allocaFromValue :: Value -> CodeGenFunction Value
allocaFromValue v = do
  t <- lift2 $ FFI.typeOf v
  name <- getValueName v
  alloca t name

br :: BasicBlock -> CodeGenFunction ()
br bb = do 
  builder <- getBuilder
  lift2 $ FFI.buildBr builder bb
  return ()

brCond :: Value -> BasicBlock -> BasicBlock -> CodeGenFunction ()
brCond v t e = do 
  builder <- getBuilder
  lift2 $ FFI.buildCondBr builder v t e
  return ()

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

int1 :: Bool -> Value
int1 b = FFI.constInt FFI.int1Type (fromIntegral v) (fromIntegral 0)
  where v = if b then 1 else 0

int32 :: Int -> Value
int32 v = FFI.constInt FFI.int32Type (fromIntegral v) (fromIntegral 1)
