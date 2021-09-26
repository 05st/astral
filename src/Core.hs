module Core where

import Base.Name
import Base.Literal
import Base.Type
import Base.Pattern
import Base.OperatorDef

type Module a = (Name, [Decl a])

data Decl a
    = DLet Name [([Pattern], Expr a)]
    | DOper OperatorDef

data Expr a
    = EApp (Expr a) (Expr a)
    | EVar Name Type
    | ELam Name Type (Expr a)
    | EMatch (Expr a) [(Pattern, Expr a)]
    | EIf (Expr a) (Expr a) (Expr a)
    | ELet Name (Expr a) (Expr a)
    | ELit Literal
    deriving (Show)

type UntypedExpr = Expr ()
type TypedExpr = Expr Type
type UntypedDecl = Decl ()
type TypedDecl = Decl Type
type UntypedModule = Module ()
type TypedModule = Module Type
