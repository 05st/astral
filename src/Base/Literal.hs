module Base.Literal where

data Literal
    = LInt Integer
    | LFloat Double
    | LBool Bool
    | LChar Char
    | LUnit
    deriving (Show)
