module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.ParserCombinators.Parsec hiding (parseTest)

import Ast
import Parser

import Data.Functor

main :: IO ()
main = defaultMain tests

tests = 
  [
    testGroup "Parser" [
       testGroup "Expression" [
          testGroup "ReservedOp" $ tests_reservedOp,
          testGroup "Literal" $ tests_literal parseExpression,
          testGroup "Variable" $ tests_variable parseExpression,
          testGroup "PrimaryExpression" $ tests_primaryExpression parseExpression,
          testGroup "PostfixExpression" $ tests_postfixExpression parseExpression,
          testGroup "UnaryExpressionNotPlusMinus" $ tests_unaryExpressionNotPlusMinus parseExpression,
          testGroup "UnaryExpression" $ tests_unaryExpression parseExpression,
          testGroup "MultiplicativeExpression" $ tests_multiplicativeExpression parseExpression,
          testGroup "AdditiveExpression" $ tests_additiveExpression parseExpression,
          testGroup "RelationalExpression" $ tests_relationalExpression parseExpression,
          testGroup "EqualitylExpression" $ tests_equalityExpression parseExpression,
          testGroup "BooleanAndExpression" $ tests_booleanAndExpression parseExpression,
          testGroup "BooleanOrExpression" $ tests_booleanOrExpression parseExpression
          ],
       testGroup "Statement" [
         testGroup "NoOp" $ tests_noOpStatement parseStatement,
         testGroup "Declaration" $ tests_declarationStatement parseStatement,
         testGroup "ExpStm" $ tests_expStmStatement parseStatement,
         testGroup "Assign" $ tests_assignStatement parseStatement,
         testGroup "If" $ tests_ifStatement parseStatement,
         testGroup "While" $ tests_whileStatement parseStatement,
         testGroup "Return" $ tests_whileStatement parseStatement,
         testGroup "Block" $ tests_whileStatement parseStatement
         ],
       testGroup "Declaration" [
         testGroup "FieldDecl" $ tests_fieldDecl  parseFieldDecl,
         testGroup "MethodDecl" $ tests_methodDecl parseMethodDecl,
         testGroup "ClassDecl" $ tests_classDecl parseClassDecl,
         testGroup "ProgramDecl" $ tests_program parseProgram
         ]
       ]
  ]

runParse :: Parser a -> String -> Either ParseError a
runParse parser string = parse parser "" string


testParseAux :: (Eq (e b), Show (e b),Functor e) =>
             (a -> b) -> Parser (e a) -> String -> e b -> Assertion
testParseAux t p s e =
  case (runParse par s) of
    Left err -> assertFailure $ show err
    Right v -> ((fmap t v) @?= e)
  where par = do r <- p
                 eof
                 return r

testParse = testParseAux id

testParseAST :: (Eq (e ()), Show (e ()), Functor e) =>
                Parser (e a) -> String -> e () -> Assertion
testParseAST = testParseAux $ const ()

--testParseProg :: (Eq (e (f ())), Show (e (f ())), Functor e, Functor f) =>
--                 Parser (e (f a)) -> String -> e (f ()) -> Assertion
--testParseProg = testParseAux $ (fmap (const ())


testParseFail :: Show a => Parser a -> String -> Assertion
testParseFail p s =   case (parse p "" s) of
    Left err  -> assertBool "" True
    Right v  -> assertFailure $ "This test should fail but got: " ++ show v

tests_reservedOp  =
  [
    testCase "resOp1" $ testParse (reservedOpR "+") "+" "+",
    testCase "resOp2" $ testParse (reservedOpR "-") "-" "-",
    testCase "resOp3" $ testParse (reservedOpR "*") "*" "*",
    testCase "resOp4" $ testParse (reservedOpR "/") "/" "/",
    testCase "resOp5" $ testParse (reservedOpR "!") "!" "!",
    testCase "resOp6" $ testParse (reservedOpR "<") "<" "<",
    testCase "resOp7" $ testParse (reservedOpR ">") ">" ">",
    testCase "resOp8" $ testParse (reservedOpR "<=") "<=" "<=",
    testCase "resOp9" $ testParse (reservedOpR ">=") ">=" ">=",
    testCase "resOp10" $ testParse (reservedOpR "==") "==" "==",
    testCase "resOp11" $ testParse (reservedOpR "&&") "&&" "&&",
    testCase "resOp12" $ testParse (reservedOpR "||") "||" "||",
    testCase "resOp13" $ testParseFail (reservedOpR "<") "<=",
    testCase "resOp14" $ testParseFail (reservedOpR ">") ">=",
    testCase "resOp15" $ testParseFail (reservedOpR "=") "==",
    testCase "resOp16" $ testParseFail (reservedOpR "!") "!=",
    testCase "resOp17" $ testParseFail (reservedOpR "|") "||",
    testCase "resOp18" $ testParseFail (reservedOpR "&") "&&",
    testCase "resOp19" $ testParse (reservedOpR "+" >> reservedOpR "-") "+-" "-",
    testCase "resOp20" $ testParse (reservedOpR "-" >> reservedOpR "-") "--" "-",
    testCase "resOp21" $ testParse (reservedOpR "*" >> reservedOpR "-") "*-" "-",
    testCase "resOp22" $ testParse (reservedOpR "/" >> reservedOpR "-") "/-" "-"
  ]

tests_literal p = 
  [
    testCase "integer" $ testParseAST p "123" (I () 123),
    testCase "integer.1" $ testParseAST p "123 " (I () 123),
    testCase "string" $ testParseAST p "\"foo\"" (S () "foo"),
    testCase "string.1" $ testParseAST p "\"foo\" " (S () "foo"),
    testCase "true" $ testParseAST p "true" (B () True),
    testCase "true.1" $ testParseAST p "true " (B () True),
    testCase "false" $ testParseAST p "false" (B () False),
    testCase "false.1" $ testParseAST p "false " (B () False),
    testCase "null" $ testParseAST p "null" (Null ()),
    testCase "null.1" $ testParseAST p "null " (Null ())
  ]

tests_variable p =  
  [
    testCase "variable" $ testParseAST p "x" (Var () "x"),
    testCase "variable.1" $ testParseAST p "x " (Var () "x"),
    testCase "this" $ testParseAST p "this" (Var () "this"),
    testCase "this.1" $ testParseAST p "this " (Var () "this")
  ]

tests_primaryExpression p = 
  tests_literal p ++ tests_variable p ++
  [
    testCase "(variable)" $ testParseAST p "(x)" (Var () "x"),
    testCase "(this)" $ testParseAST p "(this)" (Var () "this")
  ] ++
  [
    testCase "(integer)" $ testParseAST p "(123)" (I () 123),
    testCase "(string)" $ testParseAST p "(\"foo\")" (S () "foo"),
    testCase "(true)" $ testParseAST p "(true)" (B () True),
    testCase "(false)" $ testParseAST p "(false)" (B () False),
    testCase "(bool)" $ testParseFail p "(bool)",
    testCase "(int)" $ testParseFail p "(int)",
    testCase "(void)" $ testParseFail p "(void)",
    testCase "constr1" $ testParseAST p "new x()" (New () "x" []),
    testCase "constr2" $ testParseAST p "new x(1)" (New () "x" [I () 1]),
    testCase "constr3" $ testParseAST p "new x(1,2)" (New () "x" [I () 1,I () 2]),
    testCase "constr4" $ testParseFail p "new x"
  ]

tests_postfixExpression p =
  [
    testCase "field1" $ testParseAST p "x.f" (FieldAccess () "f" (Var () "x")),
    testCase "field2" $ testParseAST p "x.f.g" (FieldAccess () "g" (FieldAccess () "f" (Var () "x"))),
    testCase "field3" $ testParseAST p "x.f.g()" (MethodCall () "g" [] (FieldAccess () "f" (Var () "x"))),
    testCase "method1" $ testParseAST p "x.m()" (MethodCall () "m" [] (Var () "x")),
    testCase "method1" $ testParseAST p "x.m().n()"(MethodCall () "n" [] (MethodCall () "m" [] (Var () "x"))),
    testCase "method2" $ testParseAST p "x.m(1)" (MethodCall () "m" [I () 1] (Var () "x")),
    testCase "method3" $ testParseAST p "x.m(1,2)" (MethodCall () "m" [(I () 1),(I () 2)] (Var () "x"))
  ]

tests_unaryExpressionNotPlusMinus p =
  [
    testCase "not1" $ testParseAST p "!1" (Not () (I () 1)),
    testCase "not2" $ testParseAST p "!true" (Not () (B () True)),
    testCase "cast1" $ testParseAST p "(bool) 1" (Cast () "bool" (I () 1)),
    testCase "cast2" $ testParseAST p "(int) 1" (Cast () "int"  (I () 1)),
    testCase "cast3" $ testParseAST p "(void) 1" (Cast () "void"  (I () 1)),
    testCase "cast4" $ testParseAST p "(bool) -1" (Cast () "bool" (Negative () (I () 1))),
    testCase "cast5" $ testParseAST p "(int) -1" (Cast () "int"  (Negative () (I () 1))),
    testCase "cast6" $ testParseAST p "(void) -1" (Cast () "void" (Negative () (I () 1))),
    testCase "cast7" $ testParseAST p "(bool) !1" (Cast () "bool" (Not () (I () 1))),
    testCase "cast8" $ testParseAST p "(int) !1" (Cast () "int"  (Not () (I () 1))),
    testCase "cast9" $ testParseAST p "(void) !1" (Cast () "void" (Not () (I () 1))),
    testCase "cast10" $ testParseAST p "(bool) 1*1" (Multiplicative () "*" (Cast () "bool" (I () 1)) (I () 1)),
    testCase "cast11" $ testParseAST p "(int) 1*1" (Multiplicative () "*" (Cast () "int" (I () 1)) (I () 1)),
    testCase "cast12" $ testParseAST p "(void) 1*1" (Multiplicative () "*" (Cast () "void" (I () 1)) (I () 1)),
    testCase "cast13" $ testParseAST p "(x) 1" (Cast () "x" (I () 1)),
    testCase "cast13" $ testParseAST p "(x) (-1)" (Cast () "x" (Negative () (I () 1))),
    testCase "cast14" $ testParseAST p "(x) !1" (Cast () "x" (Not () (I () 1))),
    testCase "cast15" $ testParseAST p "(x) y" (Cast () "x" (Var () "y")),
    testCase "cast16" $ testParseAST p "(x) 1*1" (Multiplicative () "*" (Cast () "x" (I () 1)) (I () 1)),
    testCase "cast17" $ testParseAST p "(x) 1+1" (Additive () "+" (Cast () "x" (I () 1)) (I () 1)),
    testCase "cast18" $ testParseAST p "(x) 1+2*3" (Additive () "+" (Cast () "x" (I () 1)) (Multiplicative () "*" (I () 2) (I () 3))),
    testCase "cast19" $ testParseAST p "(x) 2*3+1" (Additive () "+" (Multiplicative () "*" (Cast () "x" (I () 2)) (I () 3)) (I () 1))
  ]

tests_unaryExpression p =
  [
    testCase "negative1" $ testParseAST p "-1" (Negative () (I () 1))
  ]

tests_multiplicativeExpression p =
  [
    testCase "mult1" $ testParseAST p "1*1" (Multiplicative () "*" (I () 1) (I () 1)),
    testCase "mult1.0" $ testParseAST p "1 * 1 " (Multiplicative () "*" (I () 1) (I () 1)),
    testCase "mult1.1" $ testParseAST p "-1*-1" (Multiplicative () "*" (Negative () (I () 1)) (Negative () (I () 1))),
    testCase "mult1.2" $ testParseAST p "- 1 * - 1 " (Multiplicative () "*" (Negative () (I () 1)) (Negative () (I () 1))),
    testCase "mult2" $ testParseAST p "1/1" (Multiplicative () "/" (I () 1) (I () 1)),
    testCase "mult2.1" $ testParseAST p "1 / 1 " (Multiplicative () "/" (I () 1) (I () 1)),
    testCase "mult3" $ testParseAST p "1*1*1" (Multiplicative () "*" (Multiplicative () "*" (I () 1) (I () 1)) (I () 1)),
    testCase "mult3.1" $ testParseAST p "1 * 1 * 1 " (Multiplicative () "*" (Multiplicative () "*" (I () 1) (I () 1)) (I () 1)),
    testCase "mult4" $ testParseAST p "1/1/1" (Multiplicative () "/" (Multiplicative () "/" (I () 1) (I () 1)) (I () 1)),
    testCase "mult4.1" $ testParseAST p "1 / 1 / 1 " (Multiplicative () "/" (Multiplicative () "/" (I () 1) (I () 1)) (I () 1)),
    testCase "mult5" $ testParseAST p "1*1/1" (Multiplicative () "/" (Multiplicative () "*" (I () 1) (I () 1)) (I () 1)),
    testCase "mult5.1" $ testParseAST p "1 * 1 / 1 " (Multiplicative () "/" (Multiplicative () "*" (I () 1) (I () 1)) (I () 1)),
    testCase "mult6" $ testParseAST p "1/1*1" (Multiplicative () "*" (Multiplicative () "/" (I () 1) (I () 1)) (I () 1)),
    testCase "mult6.1" $ testParseAST p "1 / 1 * 1 " (Multiplicative () "*" (Multiplicative () "/" (I () 1) (I () 1)) (I () 1)),
    testCase "mult7" $ testParseAST p "1/2*3/4" (Multiplicative () "/" (Multiplicative () "*" (Multiplicative () "/" (I () 1) (I () 2)) (I () 3)) (I () 4)),
    testCase "mult7.1" $ testParseAST p "1 / 2 * 3 / 4 " (Multiplicative () "/" (Multiplicative () "*" (Multiplicative () "/" (I () 1) (I () 2)) (I () 3)) (I () 4)),
    testCase "mult8" $ testParseAST p "(1*1)" (Multiplicative () "*" (I () 1) (I () 1)),
    testCase "mult8.1" $ testParseAST p "( 1 * 1 ) " (Multiplicative () "*" (I () 1) (I () 1)),
    testCase "mult9" $ testParseAST p "(1/1)" (Multiplicative () "/" (I () 1) (I () 1)),
    testCase "mult9.1" $ testParseAST p "( 1 / 1 ) " (Multiplicative () "/" (I () 1) (I () 1)),
    testCase "mult10" $ testParseAST p "1*(1*1)" (Multiplicative () "*" (I () 1) (Multiplicative () "*" (I () 1) (I () 1))),
    testCase "mult10.1" $ testParseAST p "1 * ( 1 * 1 )" (Multiplicative () "*" (I () 1) (Multiplicative () "*" (I () 1) (I () 1))),
    testCase "mult11" $ testParseAST p "1/(1/1)" (Multiplicative () "/" (I () 1) (Multiplicative () "/" (I () 1) (I () 1))),
    testCase "mult11.1" $ testParseAST p "1 / ( 1 / 1 ) " (Multiplicative () "/" (I () 1) (Multiplicative () "/" (I () 1) (I () 1))),
    testCase "mult12" $ testParseAST p "1*(1/1)" (Multiplicative () "*" (I () 1) (Multiplicative () "/" (I () 1) (I () 1))),
    testCase "mult12.1" $ testParseAST p "1 * ( 1 / 1 ) " (Multiplicative () "*" (I () 1) (Multiplicative () "/" (I () 1) (I () 1))),
    testCase "mult13" $ testParseAST p "1/(1*1)" (Multiplicative () "/" (I () 1) (Multiplicative () "*" (I () 1) (I () 1))),
    testCase "mult13.1" $ testParseAST p "1 / ( 1 * 1 ) " (Multiplicative () "/" (I () 1) (Multiplicative () "*" (I () 1) (I () 1))),
    testCase "mult14" $ testParseAST p "(1/2)*(3/4)" (Multiplicative () "*" (Multiplicative () "/" (I () 1) (I () 2)) (Multiplicative () "/" (I () 3) (I () 4))),
    testCase "mult14.1" $ testParseAST p "( 1 / 2 ) * ( 3 / 4 ) " (Multiplicative () "*" (Multiplicative () "/" (I () 1) (I () 2)) (Multiplicative () "/" (I () 3) (I () 4))),
    testCase "mult15" $ testParseAST p "(x)*1" (Multiplicative () "*" (Var () "x") (I () 1)),
    testCase "mult15.1" $ testParseAST p "( x ) * 1 " (Multiplicative () "*" (Var () "x") (I () 1)),
    testCase "mult16" $ testParseAST p "(x)/1" (Multiplicative () "/" (Var () "x") (I () 1)),
    testCase "mult16.1" $ testParseAST p "( x ) / 1 " (Multiplicative () "/" (Var () "x") (I () 1))
  ]

tests_additiveExpression p =
  [
    testCase "add1" $ testParseAST p "1+1" (Additive () "+" (I () 1) (I () 1)),
    testCase "add1.1" $ testParseAST p "-1+-1" (Additive () "+" (Negative () (I () 1)) (Negative () (I () 1))),
    testCase "add2" $ testParseAST p "1-1" (Additive () "-" (I () 1) (I () 1)),
    testCase "add2.1" $ testParseAST p "-1--1" (Additive () "-" (Negative () (I () 1)) (Negative () (I () 1))),
    testCase "add3" $ testParseAST p "1+1+1" (Additive () "+" (Additive () "+" (I () 1) (I () 1)) (I () 1)),
    testCase "add4" $ testParseAST p "1+2*3" (Additive () "+" (I () 1) (Multiplicative () "*" (I () 2) (I () 3))),
    testCase "add5" $ testParseAST p "2*3+1" (Additive () "+" (Multiplicative () "*" (I () 2) (I () 3)) (I () 1)),
    testCase "add6" $ testParseAST p "(1+2)*3" (Multiplicative () "*" (Additive () "+" (I () 1) (I () 2)) (I () 3)),
    testCase "add7" $ testParseAST p "3*(1+2)" (Multiplicative () "*" (I () 3) (Additive () "+" (I () 1) (I () 2))),
    testCase "add8" $ testParseAST p "(x)+1" (Additive () "+" (Var () "x") (I () 1)),
    testCase "add9" $ testParseAST p "(x)-1" (Additive () "-" (Var () "x") (I () 1))
  ]

tests_relationalExpression p =
  [
    testCase "rel1" $ testParseAST p "1<1" (Relational () "<" (I () 1) (I () 1)),
    testCase "rel2" $ testParseAST p "1<=1" (Relational () "<=" (I () 1) (I () 1)),
    testCase "rel3" $ testParseAST p "1>1" (Relational () ">" (I () 1) (I () 1)),
    testCase "rel4" $ testParseAST p "1>=1" (Relational () ">=" (I () 1) (I () 1)),
    testCase "rel5" $ testParseAST p "1<1<=1" (Relational () "<=" (Relational () "<" (I () 1) (I () 1)) (I () 1)),
    testCase "rel6" $ testParseAST p "1<2+3" (Relational () "<" (I () 1) (Additive () "+" (I () 2) (I () 3))),
    testCase "rel7" $ testParseAST p "2+3<1" (Relational () "<" (Additive () "+" (I () 2) (I () 3)) (I () 1)),
    testCase "rel8" $ testParseAST p "(1<2)+3" (Additive () "+" (Relational () "<" (I () 1) (I () 2)) (I () 3)),
    testCase "rel9" $ testParseAST p "3+(1<2)" (Additive () "+" (I () 3) (Relational () "<" (I () 1) (I () 2)))
  ]

tests_equalityExpression p =
  [
    testCase "eq1" $ testParseAST p "1==1" (Equality () "==" (I () 1) (I () 1)),
    testCase "eq2" $ testParseAST p "1!=1" (Equality () "!=" (I () 1) (I () 1)),
    testCase "eq5" $ testParseAST p "1!=1==1" (Equality () "==" (Equality () "!=" (I () 1) (I () 1)) (I () 1)),
    testCase "eq6" $ testParseAST p "1==2<3" (Equality () "==" (I () 1) (Relational () "<" (I () 2) (I () 3))),
    testCase "eq7" $ testParseAST p "2<3==1" (Equality () "==" (Relational () "<" (I () 2) (I () 3)) (I () 1)),
    testCase "eq8" $ testParseAST p "(1==2)<3" (Relational () "<" (Equality () "==" (I () 1) (I () 2)) (I () 3)),
    testCase "eq9" $ testParseAST p "3<(1==2)" (Relational () "<" (I () 3) (Equality () "==" (I () 1) (I () 2)))
  ]

tests_booleanAndExpression p =
  [
    testCase "booland1" $ testParseAST p "1&&1" (Boolean () "&&" (I () 1) (I () 1)),
    testCase "booland2" $ testParseAST p "1&&2==3" (Boolean () "&&" (I () 1) (Equality () "==" (I () 2) (I () 3))),
    testCase "booland3" $ testParseAST p "2==3&&1" (Boolean () "&&" (Equality () "==" (I () 2) (I () 3)) (I () 1)),
    testCase "booland4" $ testParseAST p "(1&&2)==3" (Equality () "==" (Boolean () "&&" (I () 1) (I () 2)) (I () 3)),
    testCase "booland5" $ testParseAST p "3==(1&&2)" (Equality () "==" (I () 3) (Boolean () "&&" (I () 1) (I () 2)))
  ]

tests_booleanOrExpression p =
  [
    testCase "boolor1" $ testParseAST p "1||1" (Boolean () "||" (I () 1) (I () 1)),
    testCase "boolor2" $ testParseAST p "1||2&&3" (Boolean () "||" (I () 1) (Boolean () "&&" (I () 2) (I () 3))),
    testCase "boolor3" $ testParseAST p "2&&3||1" (Boolean () "||" (Boolean () "&&" (I () 2) (I () 3)) (I () 1)),
    testCase "boolor4" $ testParseAST p "(1||2)&&3" (Boolean () "&&" (Boolean () "||" (I () 1) (I () 2)) (I () 3)),
    testCase "boolor5" $ testParseAST p "3&&(1||2)" (Boolean () "&&" (I () 3) (Boolean () "||" (I () 1) (I () 2)))
  ]

tests_noOpStatement p =
  [
    testCase "noOp1" $ testParseAST p ";" (NoOp ()),
    testCase "noOp" $ testParseAST p ";" (NoOp ())
  ]

tests_declarationStatement p =
  [
    testCase "declaration1" $ testParseAST p "t x;" (Declaration () "t" "x"),
    testCase "declaration2" $ testParseAST p "t x ; " (Declaration () "t" "x"),
    testCase "declaration3" $ testParseFail p "t x"
  ]

tests_expStmStatement p =
  [
    testCase "expStm1" $ testParseAST p "1;" (ExpStm (I () 1)),
    testCase "expStm2" $ testParseAST p "1 ; " (ExpStm (I () 1)),
    testCase "expStm3" $ testParseAST p "(bool) 1;" (ExpStm (Cast () "bool" (I () 1))),
    testCase "expStm4" $ testParseFail p "1"
  ]

tests_assignStatement p =
  [
    testCase "assign1" $ testParseAST p "a=1;" (Assign () (Var () "a") (I () 1)),
    testCase "assign2" $ testParseAST p "a = 1 ; " (Assign () (Var () "a") (I () 1)),
    testCase "assign3" $ testParseAST p "e.f= x;" (Assign () (FieldAccess () "f" (Var () "e")) (Var () "x")),
    testCase "assign4" $ testParseFail p "a=1"
  ]

tests_ifStatement p =
  [
    testCase "if1" $ testParseAST p "if(1);" (If (I () 1) (NoOp ()) (NoOp ())),
    testCase "if2" $ testParseAST p "if(1);else;" (If (I () 1) (NoOp ()) (NoOp ())),
    testCase "if3" $ testParseAST p "if ( 1 ) ; else ; " (If (I () 1) (NoOp ()) (NoOp ())),
    testCase "if4" $ testParseAST p "if(1) 1;" (If (I () 1) (ExpStm (I () 1)) (NoOp ())),
    testCase "if5" $ testParseAST p "if(1) 1;else 1;" (If (I () 1) (ExpStm (I () 1)) (ExpStm (I () 1))),
    testCase "if6" $ testParseAST p "if(1){}else{}" (If (I () 1) (Block []) (Block [])),
    testCase "if7" $ testParseAST p "if(1){1;}else{1;}" (If (I () 1) (Block[ExpStm (I () 1)]) (Block [ExpStm (I () 1)])),
    testCase "if8" $ testParseAST p "if(1)if(1);else 1;" (If (I () 1) (If (I () 1) (NoOp ()) (ExpStm (I () 1))) (NoOp ())),
    testCase "if9" $ testParseAST p "if(1){if(1);}else 1;" (If (I () 1) (Block [If (I () 1) (NoOp ()) (NoOp ())]) (ExpStm (I () 1)))
  ]

tests_whileStatement p =
  [
    testCase "while1" $ testParseAST p "while(1);" (While (I () 1) (NoOp ())),
    testCase "while2" $ testParseAST p "while(1) 1;" (While (I () 1) (ExpStm (I () 1))),
    testCase "while3" $ testParseAST p "while(1){1;}" (While (I () 1) (Block [ExpStm (I () 1)]))
  ]

tests_returnStatement p =
  [
    testCase "return1" $ testParseAST p "return;" (Return () (Void ())),
    testCase "retirn2" $ testParseAST p "return 1;" (Return () (I () 1))
  ]

tests_blockStatement p =
  [
    testCase "block1" $ testParseAST p "{}" (Block []),
    testCase "block2" $ testParseAST p "{;;;}" (Block [(NoOp ()),(NoOp ()),(NoOp ())]),
    testCase "block3" $ testParseAST p "{1;}" (Block [(ExpStm (I () 1))]),
    testCase "block4" $ testParseAST p "{1;1;}" (Block [(ExpStm (I () 1)),(ExpStm (I () 1))])
  ]

tests_fieldDecl p =
  [
    testCase "fieldDecl1" $ testParseAST p "t x;" (FieldDecl () "t" "x"),
    testCase "fieldDecl2" $ testParseAST p "t x ; " (FieldDecl () "t" "x"),
    testCase "fieldDecl3" $ testParseFail p "t x"
  ]

tests_methodDecl p =
  [
    testCase "methodDecl1" $ testParseAST p "t m(){}" (MethodDecl () "t" "m" [] (Block [])),
    testCase "methodDecl2" $ testParseAST p "t m(t x){}" (MethodDecl () "t" "m" [ParameterDecl () "t" "x"] (Block [])),
    testCase "methodDecl3" $ testParseAST p "t m(t x,u y){}" (MethodDecl () "t" "m" [ParameterDecl () "t" "x",ParameterDecl () "u" "y"] (Block [])),
    testCase "methodDecl4" $ testParseAST p "t m(){1;}" (MethodDecl () "t" "m" [] (Block [(ExpStm (I () 1))])),
    testCase "methodDecl5" $ testParseAST p "t m(){1;1;}" (MethodDecl () "t" "m" [] (Block [(ExpStm (I () 1)),(ExpStm (I () 1))])),
    testCase "methodDecl6" $ testParseFail p "t m()",
    testCase "methodDecl7" $ testParseFail p "t m() 1;",
    testCase "methodDecl8" $ testParseFail p "t m() return 1;",
    testCase "methodDecl9" $ testParseFail p "m(){}",
    testCase "methodDecl10" $ testParseFail p "m()",
    testCase "methodDecl11" $ testParseFail p "m() 1;",
    testCase "methodDecl12" $ testParseFail p "m() return 1;"
  ]

tests_classDecl p =
  [
    testCase "classDecl1" $ testParseAST p "class c {}" (ClassDecl () "c" "" []),
    testCase "classDecl2" $ testParseAST p "class c extends d {}" (ClassDecl () "c" "d" []),
    testCase "classDecl3" $ testParseAST p "class c extends d {t x;}" (ClassDecl () "c" "d" [(FieldDecl () "t" "x")]),
    testCase "classDecl4" $ testParseAST p "class c extends d {t x; u y;}" (ClassDecl () "c" "d" [(FieldDecl () "t" "x"),(FieldDecl () "u" "y")]),
    testCase "classDecl5" $ testParseAST p "class c extends d {t m(){}}" (ClassDecl () "c" "d" [(MethodDecl () "t" "m" [] (Block []))]),
    testCase "classDecl6" $ testParseAST p "class c extends d {t m(){} t x;}" (ClassDecl () "c" "d" [(MethodDecl () "t" "m" [] (Block [])),(FieldDecl () "t" "x")]),
    testCase "classDecl7" $ testParseAST p "class c extends d {t x; t m(){}}" (ClassDecl () "c" "d" [(FieldDecl () "t" "x"),(MethodDecl () "t" "m" [] (Block []))]),
    testCase "classDecl8" $ testParseAST p "class c extends d {t x; t m(){} u y;}" (ClassDecl () "c" "d" [(FieldDecl () "t" "x"),
                                                                                                          (MethodDecl () "t" "m" [] (Block [])),
                                                                                                          (FieldDecl () "u" "y")
                                                                                                         ]),
    testCase "classDecl8" $ testParseAST p "class c extends d {t m(){} u y; u n(){}}" (ClassDecl () "c" "d" [(MethodDecl () "t" "m" [] (Block [])),
                                                                                                             (FieldDecl () "u" "y"),
                                                                                                             (MethodDecl () "u" "n" [] (Block []))
                                                                                                            ]),
    testCase "classDecl9" $ testParseFail p "class",
    testCase "classDecl10" $ testParseFail p "class c",
    testCase "classDecl11" $ testParseFail p "class extends",
    testCase "classDecl12" $ testParseFail p "class extends {}",
    testCase "classDecl13" $ testParseFail p "class c extends {}"
  ]

tests_program p =
  [
    testCase "program1" $ testParseAST p "" (Program []),
    testCase "program2" $ testParseAST p "class c {}" (Program [(ClassDecl () "c" "" [])]),
    testCase "program3" $ testParseAST p "class c {} class d {}" (Program [(ClassDecl () "c" "" []),(ClassDecl () "d" "" [])])
  ]
