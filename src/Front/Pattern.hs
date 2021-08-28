module Front.Pattern where

import Data.Text

import Front.Name
import Front.Literal

data Pattern
    = PCon Name [Pattern]
    | PVar Name
    | PAs Name Pattern
    | PLit Literal
    | PWild
    deriving (Show)
