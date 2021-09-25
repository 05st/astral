{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module Base.Type where

import Data.Text

import Base.Name

data Type
    = TCon Name Kind
    | TVar Name Kind
    | Type :@: Type

data Kind
    = KStar
    | KVar Name
    | Kind :=> Kind
    | None
    deriving (Show)

infixr :=>
infixr :->

pattern a :-> b = (TCon "->" (KStar :=> KStar :=> KStar)) :@: a :@: b
pattern TInt = (TCon "Int" KStar)

instance Show Type where
    show ((TCon "->" _) :@: a :@: b) = '(' : show a ++ " -> " ++ show b ++ ")"
    show (TCon n _) = unpack n
    show (TVar n _) = unpack n
    show (a :@: b) = show a ++ ' ' : show b
