module Ast where

type Operation = String
type VarName = String
type TypeName = String
type FieldName = String
type MethodName = String
type ClassName = String
data Expression a = I a Int | B a Bool | S a String| Var a VarName
                  | ClassSymbol a ClassName | Null a | Void a
                  | Additive a Operation (Expression a) (Expression a)
                  | Multiplicative a Operation (Expression a) (Expression a)
                  | Relational a Operation (Expression a) (Expression a)
                  | Equality a Operation (Expression a) (Expression a)
                  | Boolean a Operation (Expression a) (Expression a)
                  | Negative a (Expression a)
                  | Not a (Expression a)
                  | Cast a TypeName (Expression a)
                  | FieldAccess a FieldName (Expression a)
                  | MethodCall a MethodName [Expression a] (Expression a)
                  | StaticMethodCall a MethodName [Expression a] ClassName
                  | New a TypeName [Expression a]
                  | DeRef a (Expression a)
                  deriving(Show,Eq)

data Statement a = NoOp a
                 | Declaration a TypeName VarName
                 | ExpStm (Expression a)
                 | Assign a (Expression a) (Expression a)
                 | If a (Expression a) (Statement a) (Statement a)
                 | While a (Expression a) (Statement a)
                 | Return a (Expression a)
                 | Block [Statement a]
                 deriving(Show,Eq)

data ParameterDecl a = ParameterDecl a TypeName VarName deriving(Show,Eq)

data FieldDecl a = FieldDecl a TypeName FieldName deriving(Show,Eq)
data MethodDecl a = MethodDecl a TypeName MethodName [ParameterDecl a] (Statement a) deriving(Show,Eq)

data ClassDecl a = ClassDecl a ClassName ClassName [FieldDecl a] [MethodDecl a] deriving(Show,Eq)

data Program a = Program [ClassDecl a] deriving(Show,Eq)

instance Functor Expression where
  fmap f (I a n) = I (f a) n
  fmap f (B a b) = B (f a) b
  fmap f (S a s) = S (f a) s
  fmap f (Var a v) = Var (f a) v
  fmap f (Null a) = Null $ f a 
  fmap f (Void a) = Void $ f a 
  fmap f (Additive a op el er) = Additive (f a) op (fmap f el) (fmap f er)
  fmap f (Multiplicative a op el er) = Multiplicative (f a) op (fmap f el) (fmap f er)
  fmap f (Relational a op el er) = Relational (f a) op (fmap f el) (fmap f er)
  fmap f (Equality a op el er) = Equality (f a) op (fmap f el) (fmap f er)
  fmap f (Boolean a op el er) = Boolean (f a) op (fmap f el) (fmap f er)
  fmap f (Negative a e) = Negative (f a) (fmap f e)
  fmap f (Not a e) = Not (f a) (fmap f e)
  fmap f (Cast a t e) = Cast (f a) t (fmap f e)
  fmap f (FieldAccess a n e) = FieldAccess (f a) n (fmap f e)
  fmap f (MethodCall a n ps e) = MethodCall (f a) n (fmap (fmap f) ps) (fmap f e)
  fmap f (New a t ps) = New (f a) t (fmap (fmap f) ps)

instance Functor Statement where
  fmap f (NoOp a)= NoOp $ f a
  fmap f (Declaration a t v) = Declaration (f a) t v
  fmap f (ExpStm e) = ExpStm $ fmap f e
  fmap f (Assign a e ev) = Assign (f a) (fmap f e) (fmap f ev)
  fmap f (If a c t e) = If (f a) (fmap f c) (fmap f t) (fmap f e)
  fmap f (While a c s) = While (f a) (fmap f c) (fmap f  s)
  fmap f (Return a e) = Return (f a) (fmap f e)
  fmap f (Block ss) = Block $ fmap (fmap f) ss

instance Functor ParameterDecl where 
  fmap f (ParameterDecl a t v) = ParameterDecl (f a) t v

instance Functor FieldDecl where
  fmap f (FieldDecl a t n) = FieldDecl (f a) t n

instance Functor MethodDecl where
  fmap f (MethodDecl a t n ps s) = MethodDecl (f a) t n (fmap (fmap f) ps) (fmap f s)

instance Functor ClassDecl where 
  fmap f (ClassDecl a n pn fd md) = ClassDecl (f a) n pn (fmap (fmap f) fd) (fmap (fmap f) md)

instance Functor Program where 
  fmap f (Program cds) = Program (fmap (fmap f) cds)

{--
instance Eq (Expression a) where
  (I _ n) == (I _ n') = n==n'
  (B _ b) == (B _ b') = b==b'
  (S _ s) == (S _ s') = s==s'
  (Var _ v) == (Var _ v') = v==v'
  (Null _) == (Null _) = True
  (Void _) == (Void _) = True
  (Additive _ op el er) == (Additive _ op' el' er') = op==op' && el==el' && er==er'
  (Multiplicative _ op el er) == (Multiplicative _ op' el' er') = op==op' && el==el' && er==er'
  (Relational _ op el er) == (Relational _ op' el' er') = op==op' && el==el' && er==er'
  (Equality _ op el er) == (Equality _ op' el' er') = op==op' && el==el' && er==er'
  (Boolean _ op el er) == (Boolean _ op' el' er') = op==op' && el==el' && er==er'
  (Negative _ e) == (Negative _ e') = e==e'
  (Not _ e) == (Not _ e') = e==e'
  (Cast _ t e) == (Cast _ t' e') = t==t' && e==e'
  (FieldAccess _ n e) == (FieldAccess _ n' e') = n==n' && e==e'
  (MethodCall _ n ps e) == (MethodCall _ n' ps' e') = n==n' && ps==ps' && e==e'
  (New _ t ps) == (New _ t' ps') = t==t' && ps==ps'
  _ == _ = False

instance Eq (Statement a) where 
  (NoOp _) == (NoOp _) = True
  (Declaration _ t v) == (Declaration _ t' v') = t==t' && v==v'
  (ExpStm _ e) == (ExpStm _ e') = e==e'
  (Assign _ e ev) == (Assign _ e' ev') = e==e' && ev==ev'
  (If _ c t e) == (If c t' e') = c==c' && t==t' && e==e'
  (While _ c s) == (While _ c' s') = c==c' && s==s'
  (Return e) == (Return e') = e==e'
  (Block ss) == (Block ss') = ss==ss' 
  _ == _ = False

instance Eq (ParameterDecl a) where 
  (ParameterDecl _ t v) == (ParameterDecl _ t' v') = t==t' && v==v'

instance Eq (MemberDecl a) where 
  (FieldDecl _ t f) == (FieldDecl _ t' f') = t==t' && f==f'
  (MethodDecl _ t m ps s) == (MethodDecl _ t' m' ps' s') = t==t' && m==m' && ps=ps' && s==s'
  _ == _ = False
--}
