module Front.Literal where

data Literal
    = LInt Integer
    | LFloat Double
    | LBool Bool
    | LChar Char
    | LString String
    | LUnit
    deriving (Show)
