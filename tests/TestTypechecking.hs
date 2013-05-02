module TestTypechecking where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.ParserCombinators.Parsec hiding (parseTest)

import Ast
import Typesystem

import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

typecheckingTests = 
  [ testGroup "TypeChecking" [
       testGroup "Expression" [
          testGroup "Literal" $ tests_ExpLiteral typeExp,
          testGroup "Var" $ tests_ExpVar typeExp,
          testGroup "BinOp" $ tests_ExpBinOp typeExp,
          testGroup "New" $ tests_ExpNew typeExp,
          testGroup "Cast" $ tests_ExpCast typeExp,
          testGroup "DeRef" $ tests_ExpDeRef typeExp,
          testGroup "FieldAccess" $ tests_ExpFieldAccess typeExp,
          testGroup "MethodCall" $  tests_ExpMethodCall typeExp
          ]
       ],
       testGroup "Typecheck" [
         testGroup "BuildClassEnv" $ tests_BuildCT buildClassTypeEnv,
         testGroup "IsWellFormed" $ tests_IsWellFormed typecheck
         ]
  ]

testTCExp :: (Eq a, Show a) => (Expression a ->  TypeExpressionEnv a (ExpressionT a)) -> 
             GlobalSymTable -> LocalSymTable -> ClassTypeEnv ->
             Expression a -> Type -> Assertion
testTCExp tc gst lst cte e t = 
  case runReaderT (evalStateT (tc e) (gst,lst)) cte of
    Left err -> assertFailure $ show err
    Right t' -> getExpType t' @?= t

testTCExpFail :: (Eq a, Show a) => (Expression a ->  TypeExpressionEnv a (ExpressionT a)) -> 
                 GlobalSymTable -> LocalSymTable -> ClassTypeEnv ->
                 Expression a -> Assertion
testTCExpFail tc gst lst cte e = 
  case runReaderT (evalStateT (tc e) (gst,lst)) cte of
    Left err -> assertBool "" True
    Right t' -> assertFailure $ "This test should fail but got: " ++ (show $ getExpType t')

testBCTE :: (Eq a, Show a) => (Program a -> BuildClassEnvComp a ()) -> 
            Program a -> ClassTypeEnv -> Assertion
testBCTE tc p cte = 
  case execStateT (tc p) buildInTypes of
    Left err -> assertFailure $ show err
    Right t' -> (t' `Map.difference` buildInTypes) @?= cte  

testBCTEFail :: (Eq a, Show a) => (Program a ->  BuildClassEnvComp a ()) -> 
            Program a -> Assertion
testBCTEFail tc p = 
  case execStateT (tc p) buildInTypes of
    Left err -> assertBool "" True
    Right t' -> assertFailure $ "This test should fail but got: " ++ show t'


testTC :: (Eq a, Show a) => (Program a ->  BaseComputation a (ProgramT a)) -> 
          Program a -> Assertion
testTC tc p = 
  case tc p of
    Left err -> assertFailure $ show err
    Right t' -> fmap (const ()) t' @?= fmap (const ()) p

testTCFail :: (Eq a, Show a) => (Program a ->  BaseComputation a (ProgramT a)) -> 
              Program a -> Assertion
testTCFail tc p = 
  case tc p of
    Left err -> assertBool "" True
    Right t' -> assertFailure $ "This test should fail but got: " ++ show t'

testTCExpEmpty :: (Eq a, Show a) => (Expression a ->  TypeExpressionEnv a (ExpressionT a)) -> 
                  Expression a -> Type -> Assertion
testTCExpEmpty tc = let me = Map.empty in testTCExp tc me me me

testTCExpEmptyFail :: (Eq a, Show a) => (Expression a ->  TypeExpressionEnv a (ExpressionT a)) -> 
                      Expression a -> Assertion
testTCExpEmptyFail tc = let me = Map.empty in testTCExpFail tc me me me

tests_ExpLiteral tc =
  [
    testCase "lit1" $ testTCExpEmpty tc (I () 0) TInt,
    testCase "lit2" $ testTCExpEmpty tc (B () True) TBool,
    testCase "lit3" $ testTCExpEmpty tc (S () "") TString,
    testCase "lit4" $ testTCExpEmpty tc (Null ()) TNull,
    testCase "lit5" $ testTCExpEmpty tc (Void ()) TVoid
  ]

tests_ExpVar tc =
  [
    testCase "var1" $ testTCExp tc me lst1 me (Var () "a") TInt,
    testCase "var2" $ testTCExpFail tc me me me (Var () "a")
  ]
  where me = Map.empty
        lst1 = Map.insert "a" TInt Map.empty

tests_ExpBinOp tc =
  [
    testCase "binop1" $ testTCExpEmpty tc (add int int) TInt,
    testCase "binop2" $ testTCExpEmptyFail tc (add int str),
    testCase "binop3" $ testTCExpEmptyFail tc (add str int),
    testCase "binop4" $ testTCExpEmpty tc (mult int int) TInt,
    testCase "binop5" $ testTCExpEmptyFail tc (mult int str),
    testCase "binop6" $ testTCExpEmptyFail tc (mult str int),
    testCase "binop7" $ testTCExpEmpty tc (rel int int) TBool,
    testCase "binop8" $ testTCExpEmptyFail tc (rel int str),
    testCase "binop9" $ testTCExpEmptyFail tc (rel str int),
    testCase "binop10" $ testTCExpEmpty tc (eq int int) TBool,
    testCase "binop11" $ testTCExpEmptyFail tc (eq int bool),
    testCase "binop12" $ testTCExpEmptyFail tc (eq bool int),
    testCase "binop13" $ testTCExpEmpty tc (eq bool bool) TBool,
    testCase "binop14" $ testTCExpEmptyFail tc (eq bool str),
    testCase "binop15" $ testTCExpEmptyFail tc (eq str bool),
    testCase "binop16" $ testTCExpEmptyFail tc (eq int str),
    testCase "binop17" $ testTCExpEmptyFail tc (eq str int),
    testCase "binop18" $ testTCExpEmpty tc (Negative () int) TInt,
    testCase "binop19" $ testTCExpEmptyFail tc (Negative () str),
    testCase "binop20" $ testTCExpEmpty tc (Not () bool) TBool,
    testCase "binop21" $ testTCExpEmptyFail tc (Not () int)
  ]
  where me = Map.empty
        int = (I () 0)
        bool = (B () True)
        str = (S () "")
        add = Additive () "+"
        mult = Multiplicative () "*"
        rel = Relational () "<"
        eq = Equality () "=="

tests_ExpNew tc = 
  [
    testCase "new1" $ testTCExp tc me me ctea (new "A" []) $ TObjId "A",
    testCase "new2" $ testTCExp tc me me cteb (new "B" [(I () 3)]) $ TObjId "B",
    testCase "new3" $ testTCExpFail tc me me cteb (new "B" []),
    testCase "new4" $ testTCExp tc me me ctec (new "C" [(I () 3),(B () True)]) $ TObjId "C",
    testCase "new5" $ testTCExpFail tc me me cteill (new "I" [(I () 3)])
  ]
  where me = Map.empty
        new = New ()
        ctea = Map.insert "A" (CT "A" "" [] me me []) me
        fB = Map.insert "f" TInt me
        fC = Map.insert "g" TBool me
        cteb = Map.insert "B" (CT "B" "" ["f"] fB me []) me
        ctec = Map.insert "C" (CT "C" "B" ["g"] fC me []) cteb
        cteill = Map.insert "I" (CT "B" "" ["f"] me me []) me
        

tests_ExpCast tc =
  [
    testCase "cast1" $ testTCExpEmpty tc (cast "int" int) TInt,
    testCase "cast2" $ testTCExpEmptyFail tc (cast "int" bool),
    testCase "cast3" $ testTCExpFail tc me me ctec (cast "C" bool),
    testCase "cast4" $ testTCExp tc me me ctebc (cast "C" $ new "B") $ TObjId "C",
    testCase "cast5" $ testTCExp tc me me cteabc (cast "C" $ new "A") $ TObjId "C",
    testCase "cast6" $ testTCExpFail tc me me ctedc (cast "D" $ new "A")
  ]
  where me = Map.empty
        cast = Cast ()
        int = (I () 0)
        bool = (B () True)
        new c = New () c []
        ctec = Map.insert "C" (CT "C" "" [] me me []) me
        ctebc = Map.insert "B" (CT "B" "C" [] me me []) ctec
        cteabc = Map.insert "A" (CT "A" "B" [] me me []) ctebc 
        ctedc = Map.insert "D" (CT "D" "C" [] me me []) cteabc
        


tests_ExpDeRef tc =
  [
    testCase "deref1" $ testTCExp tc me lst1 me (DeRef () $ Var () "a") TInt,
    testCase "deref2" $ testTCExpFail tc me lst2 me (DeRef () $ Var () "a")
  ]
  where me = Map.empty
        lst1 = Map.insert "a" (TRef TInt) Map.empty
        lst2 = Map.insert "a" (TInt) Map.empty

tests_ExpFieldAccess tc =
  [
    testCase "fieldaccess1" $ testTCExp tc me me ctea (fielda "f" (new "A" [I () 1])) TInt,
    testCase "fieldaccess2" $ testTCExp tc me me cteb (fielda "f" (new "B" [I () 1])) TInt,
    testCase "fieldaccess3" $ testTCExpFail tc me me cteb (fielda "g" (new "B" [I () 1]))
  ]
  where me = Map.empty
        new = New () 
        fielda = FieldAccess ()
        ft = Map.insert "f" TInt me
        ctea = Map.insert "A" (CT "A" "" ["f"] ft me []) me
        cteb = Map.insert "B" (CT "B" "A" [] me  me []) ctea

tests_ExpMethodCall tc =
  [
    testCase "methodcall1" $ testTCExp tc me me ctea (methodc "m" [] (new "A" [])) TInt,
    testCase "methodcall2" $ testTCExp tc me me cteb (methodc "m" [] (new "B" [])) TInt,
    testCase "methodcall3" $ testTCExp tc me me ctec (methodc "m" [] (new "C" [])) TInt,
    testCase "methodcall4" $ testTCExpFail tc me me ctea (methodc "g" [] (new "A" [])),
    testCase "methodcall5" $ testTCExpFail tc me me ctea (methodc "m" [I () 1] (new "A" [])),
    testCase "methodcall6" $ testTCExp tc me me ctea1 (methodc "m" [I () 1] (new "A" [])) TInt,
    testCase "methodcall7" $ testTCExpFail tc me me ctea1 (methodc "m" [] (new "A" [])),
    testCase "methodcall8" $ testTCExp tc me me ctea1 (methodc "m" [I () 1] (new "A" [])) TInt,
    testCase "methodcall9" $ testTCExp tc me me ctes1 (methodc "m" [] (var "S")) TInt,
    testCase "methodcall10" $ testTCExpFail tc me me ctes2 (methodc "m" [] (var "S"))
  ]
  where me = Map.empty
        new = New () 
        var = Var ()
        methodc = MethodCall ()
        mt1 = Map.insert "m" ([],TInt) me
        mt2 = Map.insert "m" ([[TInt]],TInt) me
        mt3 = Map.insert "m" ([[TBool],[TInt]],TInt) me
        ctea = Map.insert "A" (CT "A" "" [] me mt1 []) me
        cteb = Map.insert "B" (CT "B" "A" [] me  me []) ctea
        ctec = Map.insert "C" (CT "C" "B" [] me  mt1 []) cteb
        ctea1 = Map.insert "A" (CT "A" "" [] me  mt2 []) me
        ctes1 = Map.insert "S" (CT "S" "" [] me  mt1 [Static]) me
        ctes2 = Map.insert "S" (CT "S" "" [] me  mt1 []) me

{--
data ParameterDecl a = ParameterDecl a TypeName VarName deriving(Show,Eq)
data FieldDecl a = FieldDecl a TypeName FieldName deriving(Show,Eq)
data MethodDecl a = MethodDecl a TypeName MethodName [ParameterDecl a] (Statement a) deriving(Show,Eq)
data ClassDecl a = ClassDecl a ClassName ClassName [FieldDecl a] [MethodDecl a] deriving(Show,Eq)
--}

tests_BuildCT tc =
  [
    testCase "buildCT1" $ testBCTEFail tc $ p [(cd "A" "Object" [] []),(cd "A" "Object" [] [])],
    testCase "buildCT2" $ testBCTEFail tc $ p [(cd "A" "Object" [] []),(cd "A" "B" [] [])],
    testCase "buildCT3" $ testBCTEFail tc $ p [(cd "A" "Object" [(fd "int" "f"),(fd "int" "f")] [])],
    testCase "buildCT4" $ testBCTEFail tc $ p [(cd "A" "Object" [(fd "int" "f"),(fd "bool" "f")] [])],
    testCase "buildCT5" $ testBCTEFail tc $ p [(cd "A" "Object" [] [(md "int" "m" [] noop),(md "int" "m" [] noop)])],
    testCase "buildCT6" $ testBCTEFail tc $ p [(cd "A" "Object" [] [(md "int" "m" [] noop),(md "bool" "m" [] noop)])],
    testCase "buildCT7" $ testBCTE tc (p [(cd "A" "Object" [] [])]) cte1,
    testCase "buildCT8" $ testBCTE tc (p [(cd "A" "Object" [] []),(cd "B" "Object" [] [])]) cte2,
    testCase "buildCT9" $ testBCTE tc (p [(cd "A" "Object" [(fd "int" "f")] [])]) cte3,
    testCase "buildCT10" $ testBCTE tc (p [(cd "A" "Object" [] [(md "int" "m" [pd "int" "x"] noop)])]) cte4,
    testCase "buildCT11" $ testBCTE tc (p [(cd "A" "Object" [(fd "int" "f")] [(md "int" "m" [pd "int" "x"] noop)])]) cte5,
    testCase "buildCT12" $ testBCTE tc (p [(cd "A" "B" [] []),(cd "B" "C" [] []),(cd "C" "Object" [] [])]) cte6
  ]
  where p = Program
        cd = ClassDecl ()
        fd = FieldDecl ()
        md = MethodDecl ()
        noop = NoOp ()
        pd = ParameterDecl ()
        me = Map.empty
        ft = Map.insert "f" (TRef TInt) me
        mt = Map.insert "m" ([[TInt]],TInt) me
        cte1 = Map.insert "A" (CT "A" "Object" [] me me []) me
        cte2 = Map.insert "B" (CT "B" "Object" [] me me []) cte1
        cte3 = Map.insert "A" (CT "A" "Object" ["f"] ft me []) me
        cte4 = Map.insert "A" (CT "A" "Object" [] me mt []) me
        cte5 = Map.insert "A" (CT "A" "Object" ["f"] ft mt []) me
        cte6 = Map.insert "A" (CT "A" "B" [] me me []) $ Map.insert "B" (CT "B" "C" [] me me []) $ Map.insert "C" (CT "C" "Object" [] me me []) me



tests_IsWellFormed tc =
  [
    testCase "IWF1" $ testTCFail tc $ p [(cd "A" "A" [] [])],
    testCase "IWF2" $ testTCFail tc $ p [(cd "A" "B" [] []),(cd "B" "C" [] []),(cd "C" "A" [] [])],
    testCase "IWF3" $ testTCFail tc $ p [(cd "A" "B" [(fd "int" "f")] []),(cd "B" "Object" [(fd "int" "f")] [])],
    testCase "IWF4" $ testTCFail tc $ p [(cd "A" "B" [(fd "int" "f")] []),(cd "B" "Object" [(fd "bool" "f")] [])],
    testCase "IWF5" $ testTC tc $ p [(cd "A" "B" [] [(md "int" "m" [] noop)]),(cd "B" "Object" [] [(md "int" "m" [] noop)])],
    testCase "IWF6" $ testTCFail tc $ p [(cd "A" "B" [] [(md "bool" "m" [] noop)]),(cd "B" "Object" [] [(md "int" "m" [] noop)])],
    testCase "IWF7" $ testTCFail tc $ p [(cd "A" "B" [] [(md "int" "m" [pd "int" "x"] noop)]),(cd "B" "Object" [] [(md "int" "m" [] noop)])],
    testCase "IWF8" $ testTCFail tc $ p [(cd "A" "System" [] [])]
  ]
  where p = Program
        cd = ClassDecl ()
        fd = FieldDecl ()
        md = MethodDecl ()
        noop = NoOp ()
        pd = ParameterDecl ()
