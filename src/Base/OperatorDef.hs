module Base.OperatorDef where

import Base.Name

type Oper = Name

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
