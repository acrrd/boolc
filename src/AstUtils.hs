module AstUtils where

import Ast
import Control.Monad.State
import qualified Data.Set as Set

desugar :: Program a -> Program a
desugar (Program cds) = Program $ map desugarCD cds
  where desugarCD :: ClassDecl a -> ClassDecl a
        desugarCD (ClassDecl i cn pn fs ms) = ClassDecl i cn (desugarParent pn) fs $ map desugarMethod ms

        desugarParent "" = "Object"
        desugarParent s = s

        desugarMethod :: MethodDecl a -> MethodDecl a
        desugarMethod (MethodDecl i r mn ps b) = MethodDecl i r mn ps $ evalState (desugarStm b) (parametersName ps)
        
        parametersName = foldr (\(ParameterDecl _ _ vn) a -> Set.insert vn a) Set.empty


        desugarStm :: Statement a -> State (Set.Set VarName) (Statement a)
        desugarStm (NoOp i) = return $ NoOp i
        desugarStm d@(Declaration i t v) = state (\s -> (d, Set.insert v s))
        desugarStm (ExpStm e) = liftM ExpStm (desugarExp e)
        desugarStm (If i c st se) = liftM3 (If i) (desugarExp c) (desugarStm st) (desugarStm se)
        desugarStm (While i c s) = liftM2 (While i) (desugarExp c) (desugarStm s)
        desugarStm (Return i e) = liftM (Return i) (desugarExp e)
        desugarStm (Block ss) = liftM Block $ mapM desugarStm ss
        desugarStm (Assign i el er) = liftM2 (Assign i) (desugarAssign el) (desugarExp er)

        desugarAssign :: Expression a -> State (Set.Set VarName) (Expression a)
        desugarAssign e = case e of
                             FieldAccess i fn e' -> liftM (FieldAccess i fn) $ desugarExp e'
                             Var i vn -> return $ Var i vn
                             e' -> desugarExp e'

        desugarExp :: Expression a -> State (Set.Set VarName) (Expression a)
        desugarExp (Additive i op el er) = liftM2 (Additive i op) (desugarExp el) (desugarExp er)
        desugarExp (Multiplicative i op el er) = liftM2 (Multiplicative i op) (desugarExp el) (desugarExp er)
        desugarExp (Relational i op el er) = liftM2 (Relational i op) (desugarExp el) (desugarExp er)
        desugarExp (Equality i op el er) = liftM2 (Equality i op) (desugarExp el) (desugarExp er)
        desugarExp (Boolean i op el er) = liftM2 (Boolean i op) (desugarExp el) (desugarExp er)
        desugarExp (Negative i e) = liftM (Negative i) $ desugarExp e
        desugarExp (Not i e) = liftM (Not i) $ desugarExp e
        desugarExp (Cast i t e) = liftM (Cast i t) $ desugarExp e
        desugarExp (FieldAccess i fn e) = liftM (DeRef i . (FieldAccess i fn)) $ desugarExp e
        desugarExp (MethodCall i mn ps e) = liftM2 (MethodCall i mn) (mapM desugarExp ps) (desugarExp e)
        desugarExp (StaticMethodCall i mn ps cn) = liftM2 (StaticMethodCall i mn) (mapM desugarExp ps) (return cn)
        desugarExp (New i t ps) = liftM (New i t) $ mapM desugarExp ps
        desugarExp v@(Var i vn) = do s <- get
                                     if (Set.member vn s) then return $ DeRef i v 
                                                          else return v
        desugarExp e = return e
