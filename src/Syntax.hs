module Syntax where

import Data.Text

import Base.Literal
import Base.Name
import Base.Type
import Base.Pattern
import Base.OperatorDef

type Signature = Maybe Type

data Decl
    = DLetFn Name Signature [([Pattern], Expr)]
    | DLet Name Signature Expr
    | DOper OperatorDef
    deriving (Show)

data Expr
    = EVar Name
    | ELit Literal
    | EApp Expr Expr
    | ELam [Pattern] Expr
    | ELet Name Signature Expr Expr
    | EMatch Expr [(Pattern, Expr)]
    | EIf Expr Expr Expr
    | EBinOp Oper Expr Expr
    | EUnaOp Oper Expr
    | EList [Expr]
    | EString String
    deriving (Show)

type Module = (Name, [Decl])
