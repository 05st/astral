module Core where

import Base.Name
import Base.Literal
import Base.Type
import Base.Pattern

data Expr
    = EApp Expr Expr
    | EVar Name
    | ELam Name Type Expr
    | EMatch Expr [(Pattern, Expr)]
