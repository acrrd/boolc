module Ast where

type Operation = String
type TypeName = String
type FieldName = String
type MethodName = String
data Expression = I Int | B Bool | S String | Var String | Empty
                  | Additive Operation Expression Expression
                  | Multiplicative Operation Expression Expression
                  | Relational Operation Expression Expression
                  | Equality Operation Expression Expression
                  | Boolean Operation Expression Expression
                  | Negative Expression
                  | Not Expression
                  | Cast TypeName Expression
                  | FieldAccess FieldName Expression
                  | MethodCall MethodName [Expression] Expression
                  deriving(Show,Eq)
