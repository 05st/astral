module Front.Syntax where

import Data.Text

import Front.Literal
import Front.Pattern
import Front.Type
import Front.Name

type Oper = Text
type Signature = Maybe Type

data Expr
    = EVar Name
    | ELit Literal
    | EApp Expr Expr
    | ELam [Pattern] Expr
    | ELet Name Signature Expr Expr
    | EMatch Expr [(Pattern, Expr)]
    | EOper Oper [Expr]
    | EList [Expr]
    | EString String
    deriving (Show)
