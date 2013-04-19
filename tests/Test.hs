module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import TestParser
import TestTypechecking

main :: IO ()
main = do defaultMain $ parserTests ++ typecheckingTests


