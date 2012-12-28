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
       testGroup "Literal" $ tests_literal parseLiteral,
       testGroup "Variable" $ tests_variable parseVariable,
       testGroup "PrimaryExpression" $ tests_primaryExpression parsePrimaryExpression,
       testGroup "MultiplicativeExpression" $ tests_multiplicativeExpression parseMultiplicativeExpression,
       testGroup "AdditiveExpression" $ tests_additiveExpression parseAdditiveExpression,
       testGroup "RelationalExpression" $ tests_relationalExpression parseRelationalExpression,
       testGroup "EqualitylExpression" $ tests_equalityExpression parseEqualityExpression,
       testGroup "BooleanAndExpression" $ tests_booleanAndExpression parseBooleanAndExpression,
       testGroup "BooleanOrExpression" $ tests_booleanOrExpression parseBooleanOrExpression
       ]
  ]

runParse :: Parser Expression -> String -> Expression
runParse parser string = 
  case (parse parser "" string) of
    Left err  -> error $ show err
    Right v  -> v

testParse :: Parser Expression -> String -> Expression -> Assertion
testParse p s e = runParse p s @?= e

tests_literal p = 
  [
    testCase "integer" $ testParse p "123" (I 123),
    testCase "string" $ testParse p "\"foo\"" (S "foo"),
    testCase "true" $ testParse p "true" (B True),
    testCase "false" $ testParse p "false" (B False)
  ]

tests_variable p =  
  [
    testCase "variable" $ testParse p "x" (Var "x"),
    testCase "this" $ testParse p "this" (Var "this")
  ]

tests_primaryExpression p = 
  tests_literal p ++ tests_variable p ++
  [
    testCase "(variable)" $ testParse p "x" (Var "x"),
    testCase "(this)" $ testParse p "this" (Var "this")
  ] ++
  [
    testCase "(integer)" $ testParse p "123" (I 123),
    testCase "(string)" $ testParse p "\"foo\"" (S "foo"),
    testCase "(true)" $ testParse p "true" (B True),
    testCase "(false)" $ testParse p "false" (B False)
  ]

tests_multiplicativeExpression p =
  [
    testCase "mult1" $ testParse p "1*1" (Multiplicative "*" (I 1) (I 1)),
    testCase "mult2" $ testParse p "1/1" (Multiplicative "/" (I 1) (I 1)),
    testCase "mult3" $ testParse p "1*1*1" (Multiplicative "*" (Multiplicative "*" (I 1) (I 1)) (I 1)),
    testCase "mult4" $ testParse p "1/1/1" (Multiplicative "/" (Multiplicative "/" (I 1) (I 1)) (I 1)),
    testCase "mult5" $ testParse p "1*1/1" (Multiplicative "/" (Multiplicative "*" (I 1) (I 1)) (I 1)),
    testCase "mult6" $ testParse p "1/1*1" (Multiplicative "*" (Multiplicative "/" (I 1) (I 1)) (I 1)),
    testCase "mult7" $ testParse p "1/2*3/4" (Multiplicative "/" (Multiplicative "*" (Multiplicative "/" (I 1) (I 2)) (I 3)) (I 4)),
    testCase "mult8" $ testParse p "(1*1)" (Multiplicative "*" (I 1) (I 1)),
    testCase "mult9" $ testParse p "(1/1)" (Multiplicative "/" (I 1) (I 1)),
    testCase "mult10" $ testParse p "1*(1*1)" (Multiplicative "*" (I 1) (Multiplicative "*" (I 1) (I 1))),
    testCase "mult11" $ testParse p "1/(1/1)" (Multiplicative "/" (I 1) (Multiplicative "/" (I 1) (I 1))),
    testCase "mult12" $ testParse p "1*(1/1)" (Multiplicative "*" (I 1) (Multiplicative "/" (I 1) (I 1))),
    testCase "mult13" $ testParse p "1/(1*1)" (Multiplicative "/" (I 1) (Multiplicative "*" (I 1) (I 1))),
    testCase "mult14" $ testParse p "(1/2)*(3/4)" (Multiplicative "*" (Multiplicative "/" (I 1) (I 2)) (Multiplicative "/" (I 3) (I 4)))
  ]

tests_additiveExpression p =
  [
    testCase "add1" $ testParse p "1+1" (Additive "+" (I 1) (I 1)),
    testCase "add2" $ testParse p "1-1" (Additive "-" (I 1) (I 1)),
    testCase "add3" $ testParse p "1+1+1" (Additive "+" (Additive "+" (I 1) (I 1)) (I 1)),
    testCase "add4" $ testParse p "1+2*3" (Additive "+" (I 1) (Multiplicative "*" (I 2) (I 3))),
    testCase "add5" $ testParse p "2*3+1" (Additive "+" (Multiplicative "*" (I 2) (I 3)) (I 1)),
    testCase "add6" $ testParse p "(1+2)*3" (Multiplicative "*" (Additive "+" (I 1) (I 2)) (I 3)),
    testCase "add7" $ testParse p "3*(1+2)" (Multiplicative "*" (I 3) (Additive "+" (I 1) (I 2)))
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

