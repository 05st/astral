module Front.Literal where

data Literal
    = LInt Integer
    | LBool Bool
    | LChar Char
    | LUnit
    deriving (Show)
