module Typesystem where

import Ast

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

type ExpressionT a = Expression (a,Type)
type StatementT a = Statement (a,Type)
type ParameterDeclT a = ParameterDecl (a,Type)
type FieldDeclT a = FieldDecl (a,Type)
type MethodDeclT a = MethodDecl (a,Type)
type ClassDeclT a = ClassDecl (a,Type)
type ProgramT a = Program (a,Type)
  
data Modifier = Final | Static deriving(Eq,Show)

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
                 | ClassDontExist ClassName a
                 | FieldDontExist FieldName a Type
                 | MethodDontExist MethodName a Type
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
                 | NotDereferencable a Type
                 | NotStaticClass a ClassName
                 | StaticClass a ClassName
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
               Map.insert "System" (CT "System" "" [] me me [Final,Static]) me
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
                     $ throwError (ClassDontExist t i)

type CheckMemberEnv i = ReaderT ClassName (TypesystemEnv i)
type WellFormedComp i = ReaderT (Set.Set ClassName) (StateT (Set.Set ClassName) (TypesystemEnv i))

isWellFormed :: Program a -> TypesystemEnv a ()
isWellFormed prog@(Program cds) = evalStateT (runReaderT (mapM_ (isWellFormed' cds) cds) Set.empty) Set.empty

  where isWellFormed' :: [ClassDecl i] -> ClassDecl i -> WellFormedComp i ()
        isWellFormed' cds (ClassDecl i cn pn fs ms)  =
          do cycle <- asks $ Set.member cn
             when cycle $ throwError $ CyclicInheritance i cn
             checked <- gets $ Set.member cn
             if checked then return ()
               else do modify $ Set.insert cn
                       if null pn then return ()
                         else do
                           checkParent i cn cds pn
                           lift $ lift $ runReaderT (mapM_ checkField fs) cn
                           lift $ lift $ runReaderT (mapM_ checkMethod ms) cn

        checkParent :: a -> ClassName -> [ClassDecl a] -> ClassName -> WellFormedComp a ()
        checkParent i current cds pn = 
          if Map.member pn buildInTypes then return ()
            else
              case find (\(ClassDecl _ cn _ _ _) -> if pn == cn then True else False ) cds of
                Nothing -> throwError $ ClassDontExist pn i
                Just cd -> local (Set.insert current) $ isWellFormed' cds cd

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
        buildConstrArgs' cte (CT cn pn fts fm mm mods) = 
          if not $ null pn
            then case Map.lookup pn cte of
                  Just pct -> let (CT _ _ fts' _ _ _) = buildConstrArgs' cte pct in (CT cn pn (fts'++fts) fm mm mods)
                  Nothing ->  (CT cn pn fts fm mm mods) -- should never happen, run isWellFormed first!!
            else (CT cn pn fts fm mm mods) 

getExpType :: ExpressionT a -> Type
getExpType (I (_,t) _) = t
getExpType (B (_,t) _) = t
getExpType (S (_,t) _) = t
getExpType (Void (_,t)) = t
getExpType (Null (_,t)) = t
getExpType (Var (_,t) x) = t
getExpType (Additive (_,t) _ _ _) = t
getExpType (Multiplicative (_,t) _ _ _) = t
getExpType (Relational (_,t) _ _ _) = t
getExpType (Equality (_,t) _ _ _) = t
getExpType (Boolean (_,t) _ _ _) = t
getExpType (Negative (_,t) _) = t
getExpType (Not (_,t) _) = t
getExpType (Cast (_,t) _ _) = t
getExpType (FieldAccess (_,t) fn e) = t
getExpType (MethodCall (_,t) _ _ _) = t
getExpType (StaticMethodCall (_,t) _ _ _) = t
getExpType (New (_,t) _ _) = t
getExpType (DeRef (_,t) _) = t
getExpType (ClassSymbol (_,t) _) = t

typeExp :: Expression a ->  TypeExpressionEnv a (ExpressionT a)
typeExp (I i v) = return $ I (i,TInt) v
typeExp (B i v) = return $ B (i,TBool) v
typeExp (S i v) = return $ S (i,TString) v
typeExp (Void i) = return $ Void (i,TVoid)
typeExp (Null i) = return $ Null (i,TNull)
typeExp (Var i x) = do (gst,lst) <- get
                       case Map.lookup x lst of
                         Just t -> return $ Var (i,t) x
                         Nothing -> case Map.lookup x gst of
                                     Just t -> return $ Var (i,t) x
                                     Nothing -> throwError $ NotDeclaredVar x i
typeExp (Additive i op l r) = typeBinOp op i Additive l r [TInt] TInt
typeExp (Multiplicative i op l r) = typeBinOp op i Multiplicative l r [TInt] TInt
typeExp (Relational i op l r) = typeBinOp op i Relational l r [TInt] TBool
typeExp (Equality i op l r) = typeBinOp op i Equality l r [TInt,TBool] TBool
typeExp (Boolean i op l r) = typeBinOp op i Boolean l r [TBool] TBool
typeExp (Negative i e) = typeUnaryOp "-" i Negative e [TInt] TInt
typeExp (Not i e) = typeUnaryOp "!" i Not e [TBool] TBool
typeExp (Cast i t e) = do lift $ typeExist t i
                          ee <- typeExp e
                          let et = getExpType ee
                          let ct = typename2Type t
                          etct <- lift $ isSubType et ct i
                          ctet <- lift $ isSubType ct et i
                          if not (etct || ctet)
                            then throwError $ IncompatibleType i et [ct]
                            else return $ Cast (i,ct) t ee
typeExp (FieldAccess i fn e) = do ee <- typeExp e
                                  let et = getExpType ee
                                  ft <- lift $ getFieldType i et fn
                                  return $ FieldAccess (i,ft) fn ee
typeExp (MethodCall i mn ps e) = do
  ee <- typeExpGetSymbol e
  ps' <- mapM typeExp ps
  case ee of
    ClassSymbol (_,t) x -> do (ps,rt) <- typeMethodCall i (TObjId x) mn ps'
                              return $ StaticMethodCall (i,rt) mn ps x
    _ -> do let et = getExpType ee
            (ps,rt) <- typeMethodCall i et mn ps'
            return $ MethodCall (i,rt) mn ps ee
  
  where typeMethodCall i t mn ps' = do (psts,rt) <- lift $ getMethodType i t mn
                                       ps <- lift $ typeParameters i psts ps'
                                       return (ps,rt)
        typeExpGetSymbol :: Expression a ->  TypeExpressionEnv a (ExpressionT a)
        typeExpGetSymbol e = do typeExp e
                                `catchError`
                                  (\err -> case err of
                                      NotDeclaredVar _ _ -> case e of
                                        Var vi x -> do cte <- ask
                                                       when (not $ isStatic x cte) $ throwError $ NotStaticClass vi x
                                                       return $ ClassSymbol (vi,typename2Type x) x
                                        _ -> throwError err
                                      _ -> throwError err
                                  )
        isStatic cn cte = case Map.lookup cn cte of
                            Nothing -> True -- getMethodType will throw ClassDontExist
                            Just (CT _ _ _ _ _ mods) -> any (==Static) mods        

typeExp (New i cn ps) = do (CT _ _ kn fm _ mods) <- lift $ getClassType i cn
                           when (any (==Static) mods) $ throwError $ StaticClass i cn
                           kt <- lift $ getContructorType i cn
                           ps' <- mapM typeExp ps
                           ps'' <- lift $ typeParameters i [kt] ps'
                           return $ New (i,typename2Type cn) cn ps''
typeExp (DeRef i e) = do ee <- typeExp e
                         let t = getExpType ee
                         case t of
                           TRef t' -> return $ DeRef (i,t') ee
                           _ -> throwError $ NotDereferencable i t

getContrFields :: a -> ClassName -> TypesystemEnv a [FieldName]
getContrFields i cn = do (CT _ pn kn _ _ _) <- getClassType i cn
                         if not $ null pn then liftM (++kn) $ getContrFields i pn
                                          else return kn

getContructorType :: a -> ClassName -> TypesystemEnv a [Type]
getContructorType i cn =  do kns <- getContrFields i cn
                             mapM (getContrFieldType i cn) kns

  where getContrFieldType :: a -> ClassName -> FieldName -> TypesystemEnv a Type
        getContrFieldType i cn fn = getFieldType i (typename2Type cn) fn
                                    `catchError`
                                    (\e -> case e of
                                             FieldDontExist fn _ _ -> throwError $ MiscError "Illformed class type"
                                             _ -> throwError e
                                    )

getClassType :: a -> ClassName -> TypesystemEnv a ClassType
getClassType i cn = do cte <- ask
                       case Map.lookup cn cte of
                         Just ct -> return ct
                         Nothing -> throwError $ ClassDontExist cn i

getFieldType :: a -> Type -> FieldName -> TypesystemEnv a Type
getFieldType i t@(TObjId cn) fn = do (CT _ pn _ fm _ _) <- getClassType i cn
                                     case Map.lookup fn fm of
                                       Just ft -> return ft
                                       Nothing -> if not $ null pn 
                                                  then getFieldType i (typename2Type pn) fn
                                                  else throwError $ FieldDontExist fn i t
getFieldType i t fn = throwError $ FieldDontExist fn i t

getMethodType :: a -> Type -> MethodName -> TypesystemEnv a MethodType
getMethodType i t@(TObjId cn) mn = do (CT _ pn _ _ mm _) <- getClassType i cn
                                      case Map.lookup mn mm of
                                        Just mt -> return mt
                                        Nothing -> if not $ null pn
                                                   then getMethodType i (typename2Type pn) mn
                                                   else throwError $ MethodDontExist mn i t
getMethodType i t mn = throwError $ MethodDontExist mn i t

isSubType :: Type -> Type -> a -> TypesystemEnv a Bool
isSubType TNull (TObjId _) _ = return True
isSubType t t' _ | t == t' = return True
isSubType (TObjId t) b@(TObjId _) i = do (CT _ pn _ _ _ _) <- getClassType i t
                                         if null pn then return False
                                                    else isSubType (TObjId pn) b i
isSubType _ _ _  = return False


typeBinOp :: Operation -> a -> ((a,Type) -> Operation -> ExpressionT a -> ExpressionT a -> ExpressionT a) ->
             (Expression a) -> (Expression a) -> 
             [Type] -> Type -> TypeExpressionEnv a (ExpressionT a)
typeBinOp op i n l r expectedts rett = do le <- typeExp l
                                          re <- typeExp r
                                          let lt = getExpType le
                                          let rt = getExpType re
                                          when (lt /= rt || (and $ map (lt/=) expectedts))
                                            $ throwError $ InvalidBinOperandsError i op lt rt
                                          return $ n (i,rett) op le re

typeUnaryOp :: Operation -> a -> ((a,Type) -> ExpressionT a -> ExpressionT a) ->
               (Expression a) -> [Type] -> Type -> TypeExpressionEnv a (ExpressionT a)
typeUnaryOp op i n e expectedts rett = do ee <- typeExp e
                                          let et = getExpType ee
                                          when ((and $ map (et/=) expectedts))
                                            $ throwError $ InvalidUnaOperandsError i op et
                                          return $ n (i,rett) ee

typeParameters :: a -> [[Type]] -> [ExpressionT a] -> TypesystemEnv a [ExpressionT a]
typeParameters i psts ps' = do if null psts && null ps' then return []
                                 else do let pst' = map getExpType ps'
                                         typesOk <- filterM (typeParam i pst') psts
                                         when (null typesOk) $ throwError $ ParameterTypeError i psts pst'
                                         return $ fixNullType (typesOk !! 0) ps'
                                         
  where typeParam :: a -> [Type] -> [Type] -> TypesystemEnv a Bool
        typeParam i pst' pst =  do if length pst /= length pst' then return False
                                     else  foldM (\a (t',t) -> if a then isSubType (t') t i
                                                                    else return False
                                                 ) True $ zip pst' pst
        fixNullType :: [Type] -> [ExpressionT a] -> [ExpressionT a]
        fixNullType pst ps = map (\(t,e) -> case e of
                                       Null (i,t') -> Null (i,t)
                                       _ -> e
                                 ) $       
                             zip pst ps

typeStatement :: (Statement a) -> TypeStatementEnv a (StatementT a)
typeStatement (NoOp i) = return $ NoOp (i,TVoid)
typeStatement (Declaration i t vn) = do lift $ lift $ typeExist t i
                                        (gst,lst) <- lift get
                                        when (Map.member vn lst) $ throwError $ DuplicateVariable vn i
                                        cte <- lift $ ask
                                        case Map.lookup t cte  of
                                          Nothing -> throwError $ MiscError "Bug in typeExist?"
                                          Just (CT _ _ _ _ _ mods) -> when (not $ Map.member t primitiveTypesMap) $
                                                                        when (any (==Static) mods) $ throwError $ StaticClass i t
                                        let tt = TRef $ typename2Type t
                                        lift $ put (gst, Map.insert vn tt lst)
                                        return $ Declaration (i,tt) t vn
typeStatement (ExpStm e) = liftM ExpStm (lift $ typeExp e)
typeStatement (Assign i e e') = do ee <- lift $ typeExp e
                                   ee' <- lift $ typeExp e'
                                   let et = getExpType ee
                                   let et' = getExpType ee'
                                   case et' of
                                     TRef _ -> throwError $ MiscError "Did you run desugar?"
                                     _ -> case et of
                                            TRef t -> do sub <- lift $ lift $ isSubType et' t i
                                                         when (not sub) $ throwError $ IncompatibleType i et' [t]
                                                         case ee' of
                                                           Null (i,t) -> return $ Assign (i,TVoid) ee $ Null (i,t)
                                                           _ ->  return $ Assign (i,TVoid) ee ee'
                                            _ -> throwError $ LeftValueError i et
typeStatement (If i ce st se) = do ce' <- lift $ typeExp ce
                                   let cet = getExpType ce'
                                   when (cet /= TBool) $ throwError $ IncompatibleType i cet [TBool]
                                   st' <- typeStatement st
                                   se' <- typeStatement se
                                   return $ If (i,TVoid) ce' st' se'
typeStatement (Return i e ) = do ee <- lift $ typeExp e
                                 let et = getExpType ee
                                 rt <- ask
                                 sub <- lift $ lift $ isSubType et rt i
                                 when (not sub) $ throwError $ IncompatibleType i et [rt]
                                 return $ Return (i,rt) ee
typeStatement (Block ss) = liftM Block $ mapM typeStatement ss


typeProgram :: Program a -> TypesystemEnv a (ProgramT a)
typeProgram (Program cds) = liftM Program $ mapM typeClass cds
  where typeClass :: ClassDecl a -> TypesystemEnv a (ClassDeclT a)
        typeClass (ClassDecl i cn pn fs ms) = do checkParentFinal i cn pn
                                                 let cnt = typename2Type cn
                                                 let fs' = mapM (\(FieldDecl i tn n) ->
                                                                    do t <- getFieldType i cnt n
                                                                       cte <- ask
                                                                       checkNotStatic i tn cte
                                                                       return $ FieldDecl (i,t) tn n
                                                                ) fs
                                                 let ms' = mapM (typeMethod cn) ms
                                                 liftM2 (ClassDecl (i,cnt) cn pn) fs' ms'

        checkParentFinal :: a -> ClassName -> ClassName -> TypesystemEnv a ()
        checkParentFinal i cn pn = do (CT _ _ _ _ _ mod) <- getClassType i pn
                                      when (any (==Final) mod) $ throwError $ FinalParent i cn pn

        typeMethod :: ClassName -> MethodDecl a -> TypesystemEnv a (MethodDeclT a)
        typeMethod cn (MethodDecl i rtn mn ps b) =
          do cte <- ask
             checkNotStatic i rtn cte
             mapM_ (\(ParameterDecl i t _) -> checkNotStatic i t cte) ps
             let ps' = map (\(ParameterDecl i t vn) -> ParameterDecl (i,typename2Type t) t vn) ps
             let pst = foldr (\(ParameterDecl (_,t) _ vn) a -> Map.insert vn (TRef t) a) Map.empty ps'
             let lst = Map.insert "this" (typename2Type cn) $ pst
             let rt = typename2Type rtn
             let b' = evalStateT (runReaderT (typeStatement b) rt) (globalSymbols,lst)
             liftM (MethodDecl (i,rt) rtn mn ps') b'

        checkNotStatic i cn cte =
          if Map.member cn primitiveTypesMap then return ()
            else case Map.lookup cn cte  of
                   Nothing -> throwError $ MiscError "Is not well formed?"
                   Just (CT _ _ _ _ _ mods) -> do when (any (==Static) mods) $ throwError $ StaticClass i cn
                                                  return ()

typecheck :: Program a -> BaseComputation a (ProgramT a,ClassTypeEnv)
typecheck p = do cte <- execStateT (buildClassTypeEnv p) buildInTypes
                 runReaderT (isWellFormed p) cte
                 p' <- runReaderT (typeProgram p) cte
                 return (p',cte)