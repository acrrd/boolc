module Typesystem where

import Ast
import Parser (ExpressionSP,StatementSP,FieldDeclSP,MethodDeclSP,ClassDeclSP,ProgramSP)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity
import Data.List

data Type = TInt | TBool | TString | TVoid | TNull
          | TObjId ClassName | TRef Type
          deriving (Eq,Show)

data Modifier = Final deriving(Eq,Show)

data ClassType = CT ClassName ClassName [FieldName] FieldsType MethodsType [Modifier] deriving(Eq,Show)
type FieldsType = Map.Map FieldName Type
type MethodType = ([[Type]],Type)
type MethodsType = Map.Map MethodName MethodType
type ClassTypeEnv = Map.Map ClassName ClassType

data TypeError a = MultipleError [TypeError a]
                 | MiscError String
                 | DuplicateClass ClassName a
                 | DuplicateField ClassName FieldName a
                 | DuplicateMethod ClassName MethodName a
                 | DuplicateVariable VarName a
                 | ClassDontExit ClassName a
                 | FieldDontExit FieldName a
                 | MethodDontExit MethodName a
                 | FieldHiding FieldName a ClassName
                 | MethodOverload MethodName a ClassName
                 | NotDeclaredVar VarName a
                 | InvalidBinOperandsError a Operation Type Type
                 | InvalidUnaOperandsError a Operation Type
                 | IncompatibleType a Type [Type]
                 | ParameterTypeError a [[Type]] [Type]
                 | LeftValueError a Type
                 | CyclicInheritance a ClassName
                 | FinalParent a ClassName ClassName
                   deriving (Show)

instance Error (TypeError a) where
  noMsg = MiscError "Unknown error"
  strMsg str = MiscError str

type GlobalSymTable = Map.Map VarName Type
type LocalSymTable = Map.Map VarName Type

type BaseComputation i = Either (TypeError i)
type TypesystemEnv i = ReaderT ClassTypeEnv (BaseComputation i)
type TypeExpressionEnv i = StateT (GlobalSymTable,LocalSymTable) (TypesystemEnv i)
-- the env is the type used to type return
type TypeStatementEnv i = ReaderT Type (TypeExpressionEnv i)
type BuildClassEnvComp i = StateT ClassTypeEnv (BaseComputation i)

primitiveTypesMap :: Map.Map TypeName Type
primitiveTypesMap = foldr (uncurry Map.insert) Map.empty
                    [("int",TInt),("bool",TBool),("string",TString),("void",TVoid)]

typename2Type :: TypeName -> Type
typename2Type s = case Map.lookup s primitiveTypesMap of
                    Just t -> t
                    Nothing -> TObjId s

getParametersType :: [ParameterDecl a] -> [Type]
getParametersType = map (\(ParameterDecl _ t _) -> typename2Type t)

buildInTypes :: ClassTypeEnv
buildInTypes = Map.insert "Object" (CT "Object" "" [] me me []) $
               Map.insert "System" (CT "System" "" [] me me [Final]) me
  where me = Map.empty

globalSymbols :: Map.Map VarName Type
globalSymbols = Map.empty

buildClassTypeEnv :: Program i -> BuildClassEnvComp i ()
buildClassTypeEnv (Program cds) = mapM_ buildProgramEnv' cds
  where buildProgramEnv' :: ClassDecl i -> BuildClassEnvComp i ()
        buildProgramEnv' (ClassDecl i cn pn fs ms) = 
          do s <- get
             when (Map.member cn s) $ throwError (DuplicateClass cn i)
             mf <- lift $ exec buildFieldType cn fs
             mm <- lift $ exec buildMethodType cn ms
             let fts = foldr (\(FieldDecl _ _ fn) a -> fn:a) [] fs
             put $ Map.insert cn (CT cn pn fts mf mm []) s

        buildFieldType :: FieldDecl a -> 
                          ReaderT ClassName (StateT FieldsType (BaseComputation a)) ()
        buildFieldType (FieldDecl i t fn) = do fs <- get
                                               when (Map.member fn fs) 
                                                 (do cn <- ask
                                                     throwError (DuplicateField cn fn i)
                                                 )
                                               put $ Map.insert fn (TRef $ typename2Type t) fs

        buildMethodType :: MethodDecl a -> 
                          ReaderT ClassName (StateT MethodsType (BaseComputation a)) ()
        buildMethodType (MethodDecl i ret mn ps _) = do ms <- get
                                                        when (Map.member mn ms) 
                                                          (do cn <- ask
                                                              throwError (DuplicateMethod cn mn i)
                                                          )
                                                        let rt = typename2Type ret
                                                            pst = getParametersType ps
                                                        put $ Map.insert mn ([pst],rt) ms

        exec m cn xs = execStateT (runReaderT (mapM_ m xs) cn) Map.empty

typeExist :: TypeName -> a -> TypesystemEnv a ()
typeExist t i = do env <- ask
                   when (not (Map.member t env || Map.member t primitiveTypesMap))
                     $ throwError (ClassDontExit t i)

type CheckMemberEnv i = ReaderT ClassName (TypesystemEnv i)
type WellFormedComp i = ReaderT (Set.Set ClassName) (StateT (Set.Set ClassName) (TypesystemEnv i))

isWellFormed :: Program a -> TypesystemEnv a ()
isWellFormed prog@(Program cds) = evalStateT (runReaderT (mapM_ (isWellFormed' prog) cds) Set.empty) Set.empty

  where isWellFormed' :: Program i -> ClassDecl i -> WellFormedComp i ()
        isWellFormed' prog@(Program cds) (ClassDecl i cn pn fs ms)  =
          do cycle <- asks $ Set.member cn
             when cycle $ throwError $ CyclicInheritance i cn
             checked <- gets $ Set.member cn
             if checked then return ()
               else do modify $ Set.insert cn
                       if null pn then return ()
                         else if Map.member pn  buildInTypes then return ()
                                else do case find (\(ClassDecl _ cn' _ _ _) -> if pn == cn' then True else False ) cds of
                                          Nothing -> throwError $ ClassDontExit pn i
                                          Just pd -> do local (Set.insert cn) $ isWellFormed' prog pd
                                                        lift $ lift $ runReaderT (mapM_ checkField fs) cn
                                                        lift $ lift $ runReaderT (mapM_ checkMethod ms) cn

        checkMethod :: MethodDecl a -> CheckMemberEnv a ()
        checkMethod (MethodDecl i ret m ps s) = do lift $ typeExist ret i
                                                   lift $ mapM_ checkParameter ps
                                                   let rt = typename2Type ret
                                                       pst = getParametersType ps
                                                       mt = ([pst],rt)
                                                   checkMethodParent m mt i

        checkField :: FieldDecl a -> CheckMemberEnv a ()
        checkField (FieldDecl i t f) = do lift $ typeExist t i
                                          checkFieldParent f i

        checkFieldParent :: FieldName -> a -> CheckMemberEnv a ()
        checkFieldParent f i = do (CT _ pn _ fs _ _) <- getParentType
                                  when (Map.member f fs) $ throwError $ FieldHiding f i pn
                                  if not $ null pn
                                    then local (const pn) $ checkFieldParent f i
                                    else return ()

        checkMethodParent :: MethodName -> MethodType -> a -> CheckMemberEnv a ()
        checkMethodParent m mt i = do (CT _ pn _ _ ms _) <- getParentType
                                      case Map.lookup m ms of
                                        Just mt' -> when (mt /= mt') $ throwError $ MethodOverload m i pn
                                        Nothing -> if not $ null pn 
                                                     then local (const pn) $ checkMethodParent m mt i
                                                     else return ()

        checkParameter :: ParameterDecl a -> TypesystemEnv a ()
        checkParameter (ParameterDecl i t _) = typeExist t i

        getParentType :: CheckMemberEnv a (ClassType)
        getParentType = do cn <- ask
                           env <- lift ask
                           case Map.lookup cn env of
                             Just (CT _ pn _ _ _ _) ->
                               case Map.lookup pn env of
                                 Just pct -> return pct
                                 Nothing -> throwError $ MiscError "Parent class is not in the environment"
                             Nothing -> throwError $ MiscError "Current class is not in the environment"

buildConstrArgs :: ClassTypeEnv -> ClassTypeEnv
buildConstrArgs cte = Map.map (buildConstrArgs' cte) cte
  where buildConstrArgs' :: ClassTypeEnv -> ClassType -> ClassType
        buildConstrArgs' cte (CT cn pn fts fm mm mod) = 
          if not $ null pn
            then case Map.lookup pn cte of
                  Just pct -> let (CT _ _ fts' _ _ _) = buildConstrArgs' cte pct in (CT cn pn (fts'++fts) fm mm mod)
                  Nothing ->  (CT cn pn fts fm mm mod) -- should never happen, run isWellFormed first!!
            else (CT cn pn fts fm mm mod)
                                                                
typeExp :: Expression a ->  TypeExpressionEnv a Type
typeExp (I _ _) = return TInt
typeExp (B _ _) = return TBool
typeExp (S _ _) = return TString
typeExp (Void _) = return TVoid
typeExp (Null _) = return TNull
typeExp (Var i x) = do (gst,lst) <- get
                       case Map.lookup x lst of
                         Just t -> return t
                         Nothing -> case Map.lookup x gst of
                                     Just t -> return t
                                     Nothing -> throwError $ NotDeclaredVar x i
typeExp (Additive i op l r) = typeBinOp op i l r [TInt] TInt
typeExp (Multiplicative i op l r) = typeBinOp op i l r [TInt] TInt
typeExp (Relational i op l r) = typeBinOp op i l r [TInt] TBool
typeExp (Equality i op l r) = typeBinOp op i l r [TInt,TBool] TBool
typeExp (Boolean i op l r) = typeBinOp op i l r [TBool] TBool
typeExp (Negative i e) = typeUnaryOp "-" i e [TInt] TInt
typeExp (Not i e) = typeUnaryOp "!" i e [TBool] TBool
typeExp (Cast i t e) = do lift $ typeExist t i
                          et <- typeExp e
                          let ct = typename2Type t
                          etct <- lift $ isSubType et ct i
                          ctet <- lift $ isSubType ct et i
                          if not (etct || ctet)
                            then throwError $ IncompatibleType i et [ct]
                            else return ct
typeExp (FieldAccess i fn e) = do et <- typeExp e
                                  lift $ getFieldType i et fn
typeExp (MethodCall i mn ps e) = do et <- typeExp e
                                    pst' <- mapM typeExp ps
                                    (psts,rt) <- lift $ getMethodType i et mn
                                    lift $ typeParameters i psts pst'
                                    return rt

typeExp (New i cn ps) = do (CT _ _ kn fm _ _) <- lift $ getClassType i cn
                           kt <- lift $ getContructorType i cn
                           pst' <- mapM typeExp ps
                           lift $ typeParameters i [kt] pst'
                           return $ typename2Type cn
typeExp (DeRef _ e) = do t <- typeExp e
                         case t of
                           TRef t' -> return t'
                           _ -> throwError $ MiscError "DeRef applied to a non ref"

getContructorType :: a -> ClassName -> TypesystemEnv a [Type]
getContructorType i cn =  do kn <- getContrField i cn
                             mapM (getContrFieldType i cn) kn

  where getContrField :: a -> ClassName -> TypesystemEnv a [FieldName]
        getContrField i cn = do (CT _ pn kn _ _ _) <- getClassType i cn
                                if not $ null pn then liftM (++kn) $ getContrField i pn
                                                 else return kn
        getContrFieldType :: a -> ClassName -> FieldName -> TypesystemEnv a Type
        getContrFieldType i cn fn = getFieldType i (typename2Type cn) fn
                                    `catchError`
                                    (\e -> case e of
                                             FieldDontExit fn _ -> throwError $ MiscError "Illformed class type"
                                             _ -> throwError e
                                    )

getClassType :: a -> ClassName -> TypesystemEnv a ClassType
getClassType i cn = do cte <- ask
                       case Map.lookup cn cte of
                         Just ct -> return ct
                         Nothing -> throwError $ ClassDontExit cn i

getFieldType :: a -> Type -> FieldName -> TypesystemEnv a Type
getFieldType i (TObjId cn) fn = do (CT _ pn _ fm _ _) <- getClassType i cn
                                   case Map.lookup fn fm of
                                     Just ft -> return ft
                                     Nothing -> if not $ null pn 
                                                then getFieldType i (typename2Type pn) fn
                                                else throwError $ FieldDontExit fn i
getFieldType _ _ _ = throwError $ MiscError "Type is not referring to a class"

getMethodType :: a -> Type -> MethodName -> TypesystemEnv a MethodType
getMethodType i (TObjId cn) mn = do (CT _ pn _ _ mm _) <- getClassType i cn
                                    case Map.lookup mn mm of
                                      Just mt -> return mt
                                      Nothing -> if not $ null pn
                                                 then getMethodType i (typename2Type pn) mn
                                                 else throwError $ MethodDontExit mn i
getMethodType _ _ _ = throwError $ MiscError "Type is not referring to a class"


isSubType :: Type -> Type -> a -> TypesystemEnv a Bool
isSubType TNull (TObjId _) _ = return True
isSubType t t' _ | t == t' = return True
isSubType (TObjId t) b@(TObjId _) i = do (CT _ pn _ _ _ _) <- getClassType i t
                                         if null pn then return False
                                                    else isSubType (TObjId pn) b i
isSubType _ _ _  = return False


typeBinOp :: Operation -> a -> (Expression a) -> (Expression a) -> [Type] -> Type ->
             TypeExpressionEnv a Type
typeBinOp op i l r expectedts rett = do lt <- typeExp l
                                        rt <- typeExp r
                                        when (lt /= rt || (and $ map (lt/=) expectedts))
                                          $ throwError $ InvalidBinOperandsError i op lt rt
                                        return rett

typeUnaryOp :: Operation -> a -> (Expression a) -> [Type] -> Type -> TypeExpressionEnv a Type
typeUnaryOp op i e expectedts rett = do et <- typeExp e
                                        when ((and $ map (et/=) expectedts))
                                          $ throwError $ InvalidUnaOperandsError i op et
                                        return rett

typeParameters :: a -> [[Type]] -> [Type] -> TypesystemEnv a ()
typeParameters i psts pst' = do if null psts && null pst' then return ()
                                  else do typed <- foldM (\a pst -> if a then return True
                                                                         else typeParam i pst pst'
                                                         ) False psts
                                          when (not typed) $ throwError $ ParameterTypeError i psts pst'
  where typeParam :: a -> [Type] -> [Type] -> TypesystemEnv a Bool
        typeParam i pst pst' =  do if length pst /= length pst' then return False
                                     else  foldM (\a (t',t) -> if a then isSubType t' t i
                                                                    else return False     
                                                 ) True $ zip pst' pst

typeStatement :: (Statement a) -> TypeStatementEnv a ()
typeStatement (NoOp _) = return ()
typeStatement (Declaration i t vn) = do lift $ lift $ typeExist t i
                                        (gst,lst) <- lift get
                                        when (Map.member vn lst) $ throwError $ DuplicateVariable vn i
                                        lift $ put (gst, Map.insert vn (TRef $ typename2Type t) lst)
typeStatement (ExpStm e) = (lift $ typeExp e) >> return ()
typeStatement (Assign i e e') = do et <- lift $ typeExp e
                                   et' <- lift $ typeExp e'
                                   case et' of
                                     TRef _ -> throwError $ MiscError "Did you run desugar?"
                                     _ -> case et of
                                            TRef t -> do sub <- lift $ lift $ isSubType et' t i
                                                         when (not sub) $ throwError $ IncompatibleType i et' [t]
                                            _ -> throwError $ LeftValueError i et
typeStatement (If i ce st se) = do cet <- lift $ typeExp ce
                                   when (cet /= TBool) $ throwError $ IncompatibleType i cet [TBool]
                                   typeStatement st
                                   typeStatement se
typeStatement (Return i e ) = do et <- lift $ typeExp e
                                 rt <- ask
                                 sub <- lift $ lift $ isSubType et rt i
                                 when (not sub) $ throwError $ IncompatibleType i et [rt]
typeStatement (Block ss) = mapM_ typeStatement ss


typeProgram :: Program a -> TypesystemEnv a ()
typeProgram (Program cds) = mapM_ typeClass cds
  where typeClass :: ClassDecl a -> TypesystemEnv a ()
        typeClass (ClassDecl i cn pn _ ms) = do checkParentFinal i cn pn
                                             
                                                mapM_ (typeMethod cn) ms

        checkParentFinal :: a -> ClassName -> ClassName -> TypesystemEnv a ()
        checkParentFinal i cn pn = do (CT _ _ _ _ _ mod) <- getClassType i pn
                                      when (any (==Final) mod) $ throwError $ FinalParent i cn pn

        typeMethod :: ClassName -> MethodDecl a -> TypesystemEnv a ()
        typeMethod cn (MethodDecl _ rt _ ps b) =
          do cte <- ask
             let pst = foldr (\(ParameterDecl _ t vn) a -> Map.insert vn (TRef $ typename2Type t) a) Map.empty ps
             let lst = Map.insert "this" (typename2Type cn) pst
             runStateT (runReaderT (typeStatement b) (typename2Type rt)) (globalSymbols,lst)
             return ()

typecheck :: Program a -> BaseComputation a ()
typecheck p = do cte <- execStateT (buildClassTypeEnv p) buildInTypes
                 runReaderT (isWellFormed p) cte
                 runReaderT (typeProgram p) cte
