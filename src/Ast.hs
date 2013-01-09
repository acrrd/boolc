module Ast where

type Operation = String
type VarName = String
type TypeName = String
type FieldName = String
type MethodName = String
data Expression = I Int | B Bool | S String | Var VarName
                  | Null | Void
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
                  | New TypeName [Expression]
                  deriving(Show,Eq)

data Statement = NoOp 
               | Declaration TypeName VarName
               | ExpStm Expression
               | Assign Expression Expression
               | If Expression Statement Statement
               | While Expression Statement
               | Return Expression
               | Block [Statement]
               deriving(Show,Eq)
