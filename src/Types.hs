module Types where

data Type = TInt | TBool | TString | TVoid | TNull
          | TObjId String | TRef Type
          deriving (Eq,Ord,Show)

