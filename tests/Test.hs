module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.ParserCombinators.Parsec hiding (parseTest)
import Ast
import Parser


main :: IO ()
main = defaultMain tests

tests = 
  [
    testGroup "Parser" [
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
       ]
  ]

runParse :: Parser a -> String -> Either ParseError a
runParse parser string = parse parser "" string

testParse :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParse p s e =
  case (runParse par s) of
    Left err  -> assertFailure $ show err
    Right v  -> v @?= e
  where par = do r <- p
                 eof
                 return r

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
    testCase "integer" $ testParse p "123" (I 123),
    testCase "integer.1" $ testParse p "123 " (I 123),
    testCase "string" $ testParse p "\"foo\"" (S "foo"),
    testCase "string.1" $ testParse p "\"foo\" " (S "foo"),
    testCase "true" $ testParse p "true" (B True),
    testCase "true.1" $ testParse p "true " (B True),
    testCase "false" $ testParse p "false" (B False),
    testCase "false.1" $ testParse p "false " (B False)
  ]

tests_variable p =  
  [
    testCase "variable" $ testParse p "x" (Var "x"),
    testCase "variable.1" $ testParse p "x " (Var "x"),
    testCase "this" $ testParse p "this" (Var "this"),
    testCase "this.1" $ testParse p "this " (Var "this")
  ]

tests_primaryExpression p = 
  tests_literal p ++ tests_variable p ++
  [
    testCase "(variable)" $ testParse p "(x)" (Var "x"),
    testCase "(this)" $ testParse p "(this)" (Var "this")
  ] ++
  [
    testCase "(integer)" $ testParse p "(123)" (I 123),
    testCase "(string)" $ testParse p "(\"foo\")" (S "foo"),
    testCase "(true)" $ testParse p "(true)" (B True),
    testCase "(false)" $ testParse p "(false)" (B False),
    testCase "(bool)" $ testParseFail p "(bool)",
    testCase "(int)" $ testParseFail p "(int)",
    testCase "(void)" $ testParseFail p "(void)",
    testCase "constr1" $ testParse p "new x()" (New "x" []),
    testCase "constr2" $ testParse p "new x(1)" (New "x" [I 1]),
    testCase "constr3" $ testParse p "new x(1,2)" (New "x" [I 1,I 2]),
    testCase "constr4" $ testParseFail p "new x"
  ]

tests_postfixExpression p =
  [
    testCase "field1" $ testParse p "x.f" (FieldAccess "f" (Var "x")),
    testCase "field2" $ testParse p "x.f.g" (FieldAccess "g" (FieldAccess "f" (Var "x"))),
    testCase "field3" $ testParse p "x.f.g()" (MethodCall "g" [] (FieldAccess "f" (Var "x"))),
    testCase "method1" $ testParse p "x.m()" (MethodCall "m" [] (Var "x")),
    testCase "method1" $ testParse p "x.m().n()"(MethodCall "n" [] (MethodCall "m" [] (Var "x"))),
    testCase "method2" $ testParse p "x.m(1)" (MethodCall "m" [I 1] (Var "x")),
    testCase "method3" $ testParse p "x.m(1,2)" (MethodCall "m" [(I 1),(I 2)] (Var "x"))
  ]

tests_unaryExpressionNotPlusMinus p =
  [
    testCase "not1" $ testParse p "!1" (Not (I 1)),
    testCase "not2" $ testParse p "!true" (Not (B True)),
    testCase "cast1" $ testParse p "(bool) 1" (Cast "bool" (I 1)),
    testCase "cast2" $ testParse p "(int) 1" (Cast "int"  (I 1)),
    testCase "cast3" $ testParse p "(void) 1" (Cast "void"  (I 1)),
    testCase "cast4" $ testParse p "(bool) -1" (Cast "bool" (Negative (I 1))),
    testCase "cast5" $ testParse p "(int) -1" (Cast "int"  (Negative (I 1))),
    testCase "cast6" $ testParse p "(void) -1" (Cast "void" (Negative (I 1))),
    testCase "cast7" $ testParse p "(bool) !1" (Cast "bool" (Not (I 1))),
    testCase "cast8" $ testParse p "(int) !1" (Cast "int"  (Not (I 1))),
    testCase "cast9" $ testParse p "(void) !1" (Cast "void" (Not (I 1))),
    testCase "cast10" $ testParse p "(bool) 1*1" (Multiplicative "*" (Cast "bool" (I 1)) (I 1)),
    testCase "cast11" $ testParse p "(int) 1*1" (Multiplicative "*" (Cast "int" (I 1)) (I 1)),
    testCase "cast12" $ testParse p "(void) 1*1" (Multiplicative "*" (Cast "void" (I 1)) (I 1)),
    testCase "cast13" $ testParse p "(x) 1" (Cast "x" (I 1)),
    testCase "cast13" $ testParse p "(x) (-1)" (Cast "x" (Negative (I 1))),
    testCase "cast14" $ testParse p "(x) !1" (Cast "x"  (Not (I 1))),
    testCase "cast15" $ testParse p "(x) y" (Cast "x" (Var "y")),
    testCase "cast16" $ testParse p "(x) 1*1" (Multiplicative "*" (Cast "x" (I 1)) (I 1)),
    testCase "cast17" $ testParse p "(x) 1+1" (Additive "+" (Cast "x" (I 1)) (I 1)),
    testCase "cast18" $ testParse p "(x) 1+2*3" (Additive "+" (Cast "x" (I 1)) (Multiplicative "*" (I 2) (I 3))),
    testCase "cast19" $ testParse p "(x) 2*3+1" (Additive "+" (Multiplicative "*" (Cast "x" (I 2)) (I 3)) (I 1))
  ]

tests_unaryExpression p =
  [
    testCase "negative1" $ testParse p "-1" (Negative (I 1))
  ]

tests_multiplicativeExpression p =
  [
    testCase "mult1" $ testParse p "1*1" (Multiplicative "*" (I 1) (I 1)),
    testCase "mult1.0" $ testParse p "1 * 1 " (Multiplicative "*" (I 1) (I 1)),
    testCase "mult1.1" $ testParse p "-1*-1" (Multiplicative "*" (Negative (I 1)) (Negative (I 1))),
    testCase "mult1.2" $ testParse p "- 1 * - 1 " (Multiplicative "*" (Negative (I 1)) (Negative (I 1))),
    testCase "mult2" $ testParse p "1/1" (Multiplicative "/" (I 1) (I 1)),
    testCase "mult2.1" $ testParse p "1 / 1 " (Multiplicative "/" (I 1) (I 1)),
    testCase "mult3" $ testParse p "1*1*1" (Multiplicative "*" (Multiplicative "*" (I 1) (I 1)) (I 1)),
    testCase "mult3.1" $ testParse p "1 * 1 * 1 " (Multiplicative "*" (Multiplicative "*" (I 1) (I 1)) (I 1)),
    testCase "mult4" $ testParse p "1/1/1" (Multiplicative "/" (Multiplicative "/" (I 1) (I 1)) (I 1)),
    testCase "mult4.1" $ testParse p "1 / 1 / 1 " (Multiplicative "/" (Multiplicative "/" (I 1) (I 1)) (I 1)),
    testCase "mult5" $ testParse p "1*1/1" (Multiplicative "/" (Multiplicative "*" (I 1) (I 1)) (I 1)),
    testCase "mult5.1" $ testParse p "1 * 1 / 1 " (Multiplicative "/" (Multiplicative "*" (I 1) (I 1)) (I 1)),
    testCase "mult6" $ testParse p "1/1*1" (Multiplicative "*" (Multiplicative "/" (I 1) (I 1)) (I 1)),
    testCase "mult6.1" $ testParse p "1 / 1 * 1 " (Multiplicative "*" (Multiplicative "/" (I 1) (I 1)) (I 1)),
    testCase "mult7" $ testParse p "1/2*3/4" (Multiplicative "/" (Multiplicative "*" (Multiplicative "/" (I 1) (I 2)) (I 3)) (I 4)),
    testCase "mult7.1" $ testParse p "1 / 2 * 3 / 4 " (Multiplicative "/" (Multiplicative "*" (Multiplicative "/" (I 1) (I 2)) (I 3)) (I 4)),
    testCase "mult8" $ testParse p "(1*1)" (Multiplicative "*" (I 1) (I 1)),
    testCase "mult8.1" $ testParse p "( 1 * 1 ) " (Multiplicative "*" (I 1) (I 1)),
    testCase "mult9" $ testParse p "(1/1)" (Multiplicative "/" (I 1) (I 1)),
    testCase "mult9.1" $ testParse p "( 1 / 1 ) " (Multiplicative "/" (I 1) (I 1)),
    testCase "mult10" $ testParse p "1*(1*1)" (Multiplicative "*" (I 1) (Multiplicative "*" (I 1) (I 1))),
    testCase "mult10.1" $ testParse p "1 * ( 1 * 1 )" (Multiplicative "*" (I 1) (Multiplicative "*" (I 1) (I 1))),
    testCase "mult11" $ testParse p "1/(1/1)" (Multiplicative "/" (I 1) (Multiplicative "/" (I 1) (I 1))),
    testCase "mult11.1" $ testParse p "1 / ( 1 / 1 ) " (Multiplicative "/" (I 1) (Multiplicative "/" (I 1) (I 1))),
    testCase "mult12" $ testParse p "1*(1/1)" (Multiplicative "*" (I 1) (Multiplicative "/" (I 1) (I 1))),
    testCase "mult12.1" $ testParse p "1 * ( 1 / 1 ) " (Multiplicative "*" (I 1) (Multiplicative "/" (I 1) (I 1))),
    testCase "mult13" $ testParse p "1/(1*1)" (Multiplicative "/" (I 1) (Multiplicative "*" (I 1) (I 1))),
    testCase "mult13.1" $ testParse p "1 / ( 1 * 1 ) " (Multiplicative "/" (I 1) (Multiplicative "*" (I 1) (I 1))),
    testCase "mult14" $ testParse p "(1/2)*(3/4)" (Multiplicative "*" (Multiplicative "/" (I 1) (I 2)) (Multiplicative "/" (I 3) (I 4))),
    testCase "mult14.1" $ testParse p "( 1 / 2 ) * ( 3 / 4 ) " (Multiplicative "*" (Multiplicative "/" (I 1) (I 2)) (Multiplicative "/" (I 3) (I 4))),
    testCase "mult15" $ testParse p "(x)*1" (Multiplicative "*" (Var "x") (I 1)),
    testCase "mult15.1" $ testParse p "( x ) * 1 " (Multiplicative "*" (Var "x") (I 1)),
    testCase "mult16" $ testParse p "(x)/1" (Multiplicative "/" (Var "x") (I 1)),
    testCase "mult16.1" $ testParse p "( x ) / 1 " (Multiplicative "/" (Var "x") (I 1))
  ]

tests_additiveExpression p =
  [
    testCase "add1" $ testParse p "1+1" (Additive "+" (I 1) (I 1)),
    testCase "add1.1" $ testParse p "-1+-1" (Additive "+" (Negative (I 1)) (Negative (I 1))),
    testCase "add2" $ testParse p "1-1" (Additive "-" (I 1) (I 1)),
    testCase "add2.1" $ testParse p "-1--1" (Additive "-" (Negative (I 1)) (Negative (I 1))),
    testCase "add3" $ testParse p "1+1+1" (Additive "+" (Additive "+" (I 1) (I 1)) (I 1)),
    testCase "add4" $ testParse p "1+2*3" (Additive "+" (I 1) (Multiplicative "*" (I 2) (I 3))),
    testCase "add5" $ testParse p "2*3+1" (Additive "+" (Multiplicative "*" (I 2) (I 3)) (I 1)),
    testCase "add6" $ testParse p "(1+2)*3" (Multiplicative "*" (Additive "+" (I 1) (I 2)) (I 3)),
    testCase "add7" $ testParse p "3*(1+2)" (Multiplicative "*" (I 3) (Additive "+" (I 1) (I 2))),
    testCase "add8" $ testParse p "(x)+1" (Additive "+" (Var "x") (I 1)),
    testCase "add9" $ testParse p "(x)-1" (Additive "-" (Var "x") (I 1))
  ]

tests_relationalExpression p =
  [
    testCase "rel1" $ testParse p "1<1" (Relational "<" (I 1) (I 1)),
    testCase "rel2" $ testParse p "1<=1" (Relational "<=" (I 1) (I 1)),
    testCase "rel3" $ testParse p "1>1" (Relational ">" (I 1) (I 1)),
    testCase "rel4" $ testParse p "1>=1" (Relational ">=" (I 1) (I 1)),
    testCase "rel5" $ testParse p "1<1<=1" (Relational "<=" (Relational "<" (I 1) (I 1)) (I 1)),
    testCase "rel6" $ testParse p "1<2+3" (Relational "<" (I 1) (Additive "+" (I 2) (I 3))),
    testCase "rel7" $ testParse p "2+3<1" (Relational "<" (Additive "+" (I 2) (I 3)) (I 1)),
    testCase "rel8" $ testParse p "(1<2)+3" (Additive "+" (Relational "<" (I 1) (I 2)) (I 3)),
    testCase "rel9" $ testParse p "3+(1<2)" (Additive "+" (I 3) (Relational "<" (I 1) (I 2)))
  ]

tests_equalityExpression p =
  [
    testCase "eq1" $ testParse p "1==1" (Equality "==" (I 1) (I 1)),
    testCase "eq2" $ testParse p "1!=1" (Equality "!=" (I 1) (I 1)),
    testCase "eq5" $ testParse p "1!=1==1" (Equality "==" (Equality "!=" (I 1) (I 1)) (I 1)),
    testCase "eq6" $ testParse p "1==2<3" (Equality "==" (I 1) (Relational "<" (I 2) (I 3))),
    testCase "eq7" $ testParse p "2<3==1" (Equality "==" (Relational "<" (I 2) (I 3)) (I 1)),
    testCase "eq8" $ testParse p "(1==2)<3" (Relational "<" (Equality "==" (I 1) (I 2)) (I 3)),
    testCase "eq9" $ testParse p "3<(1==2)" (Relational "<" (I 3) (Equality "==" (I 1) (I 2)))
  ]

tests_booleanAndExpression p =
  [
    testCase "booland1" $ testParse p "1&&1" (Boolean "&&" (I 1) (I 1)),
    testCase "booland2" $ testParse p "1&&2==3" (Boolean "&&" (I 1) (Equality "==" (I 2) (I 3))),
    testCase "booland3" $ testParse p "2==3&&1" (Boolean "&&" (Equality "==" (I 2) (I 3)) (I 1)),
    testCase "booland4" $ testParse p "(1&&2)==3" (Equality "==" (Boolean "&&" (I 1) (I 2)) (I 3)),
    testCase "booland5" $ testParse p "3==(1&&2)" (Equality "==" (I 3) (Boolean "&&" (I 1) (I 2)))
  ]

tests_booleanOrExpression p =
  [
    testCase "boolor1" $ testParse p "1||1" (Boolean "||" (I 1) (I 1)),
    testCase "boolor2" $ testParse p "1||2&&3" (Boolean "||" (I 1) (Boolean "&&" (I 2) (I 3))),
    testCase "boolor3" $ testParse p "2&&3||1" (Boolean "||" (Boolean "&&" (I 2) (I 3)) (I 1)),
    testCase "boolor4" $ testParse p "(1||2)&&3" (Boolean "&&" (Boolean "||" (I 1) (I 2)) (I 3)),
    testCase "boolor5" $ testParse p "3&&(1||2)" (Boolean "&&" (I 3) (Boolean "||" (I 1) (I 2)))
  ]

