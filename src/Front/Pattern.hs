module Front.Pattern where

import Data.Text

import Front.Name
import Front.Literal

data Pattern
    = PatCon Name [Pattern]
    | PatVar Name
    | PatAs Name Pattern
    | PatLit Literal
    | PatWild
