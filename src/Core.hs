module Core where

import Base.Name
import Base.Literal
import Base.Type
import Base.Pattern
import Base.OperatorDef

type Module a = (Name, [Decl a])
type Signature = Maybe Type

data Decl a
    = DLet Name Signature [([Pattern], Expr a)]
    | DOper OperatorDef
    deriving (Show)

data Expr a
    = EApp a (Expr a) (Expr a)
    | EVar a Name
    | ELam a Name (Expr a)
    | EMatch a (Expr a) [(Pattern, Expr a)]
    | EIf a (Expr a) (Expr a) (Expr a)
    | ELet a Name (Expr a) (Expr a)
    | ELit a Literal
    deriving (Show)

type UntypedExpr = Expr ()
type TypedExpr = Expr Type
type UntypedDecl = Decl ()
type TypedDecl = Decl Type
type UntypedModule = Module ()
type TypedModule = Module Type
