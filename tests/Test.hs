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
       testGroup "AdditiveExpression" $ tests_additiveExpression parseAdditiveExpression
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
