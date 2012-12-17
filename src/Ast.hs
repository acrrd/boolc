module Ast where

type Operation = String
data Expression i = I Int | B Bool | S String | Var i
                  | Additive Operation (Expression i) (Expression i)
                  | Multiplicative Operation (Expression i) (Expression i)
                  deriving(Show)

