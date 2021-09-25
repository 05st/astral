module Front.Syntax where

import Data.Text

import Front.Literal
import Front.Pattern
import Front.Type
import Front.Name

type Oper = Text
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

data Assoc
    = ALeft
    | ARight
    | ANone
    | APrefix
    | APostfix
    deriving (Show)

data OperatorDef = OperatorDef
    { assoc :: Assoc
    , prec :: Integer
    , oper :: Oper
    } deriving (Show)

type Module = (Name, [Decl])
