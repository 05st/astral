module Base.Pattern where

import Data.Text

import Base.Name
import Base.Literal

data Pattern
    = PCon Name [Pattern]
    | PVar Name
    | PAs Name Pattern
    | PLit Literal
    | PWild
    deriving (Show)
