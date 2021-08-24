{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module Front.Type where

import Data.Text

import Front.Name

data Type
    = TCon Name Kind
    | TVar Name Kind
    | Type :@: Type

data Kind
    = KStar
    | KVar Name
    | Kind :=> Kind
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
