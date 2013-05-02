module ToLLVM where

import Ast
import qualified Typesystem as T
import LLVM

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

type FieldOffSets = M.Map FieldName Int
type MethodOffSets = M.Map MethodName Int
data ClassInfo = ClassInfo { classType :: Type, fieldOffsets :: FieldOffSets,
                             methodOffsets :: MethodOffSets }

type ClassInfoEnv = (M.Map ClassName ClassInfo)
type ClassInfoEnvComp = ReaderT T.ClassTypeEnv (StateT (ClassInfoEnv,S.Set ClassName) IO)

genClassNameSym :: String -> String
genClassNameSym = ("struct." ++)


buildClassValueEnv :: T.ClassTypeEnv -> IO ClassInfoEnv
buildClassValueEnv cte = execStateT (runReaderT (mapM_ buildCVE (M.keys cte)) cte) $ buildBaseCIE cte
 where buildCVE :: ClassName -> ClassInfoEnvComp ()
       buildCVE cn = return ()
       
       buildBaseCIE :: T.ClassTypeEnv -> IO ClassInfoEnv
       buildBaseCIE cte = liftM  M.fromList (mapM makePair $ M.keys cte)

       makePair :: ClassName -> IO (ClassName,ClassInfo)
       makePair k = do t <- structCreateNamed k
                       return $ (genClassNameSym k,ClassInfo t M.empty M.empty)
      {-- do
         (cie,visited) <- get
         if S.member cn visited then return ()
           else do 
             cte <- ask
             case M.lookup cn cte of
               Nothing -> error "ClassName taken from cte keys is not in cte"
               Just (T.CT _ pn fns ftm mtm _ ) -> do
                 if null pn then do struct <- genStructBody
                                    
                 when (not $ S.member pn visited) $ buildCVE pn
                 case M.lookup pn cie of
                   Nothing -> error "Parent info must be in the environment"
                   Just pi -> do 
                     case M.lookup cn cie of
                       Nothing -> error "Malformed cie"
                       Just ci -> genClassInfo 
                                   


       getBaseOffset :: MethodOffSets -> Int
       getBaseOffset mo = let offsets = M.elems mo in
         if null (M.elems mo) then 0 else maximum offsets + 1

       genStructBody :: [FieldName] -> T.FieldsType -> T.MethodsType -> [Type]
       genStructBody fns ftm mtm = genStructBody
--}