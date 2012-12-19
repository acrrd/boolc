module Ast where

type Operation = String
data Expression = I Int | B Bool | S String | Var String | Empty
                  | Additive Operation Expression Expression
                  | Multiplicative Operation Expression Expression
                  deriving(Show)

