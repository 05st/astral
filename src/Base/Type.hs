{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module Base.Type where

import qualified Data.Set as Set
import qualified Data.Text as Text

import Base.Name

data TVar
    = TV Name Kind
    deriving (Show, Eq, Ord)

data Type
    = TCon Name Kind
    | TVar TVar
    | Type :@: Type

data Constraint = CEqual Type Type | CClass Type (Set.Set String)

data TypeScheme
    = Forall (Set.Set TVar) Type deriving (Show)

data Kind
    = KStar
    | KVar Name
    | Kind :=> Kind
    | None
    deriving (Show, Eq, Ord)

infixr :=>
infixr :->

pattern a :-> b = (TCon "->" (KStar :=> KStar :=> KStar)) :@: a :@: b
pattern TInt = (TCon "Int" KStar)

instance Show Type where
    show ((TCon "->" _) :@: a :@: b) = '(' : show a ++ " -> " ++ show b ++ ")"
    show (TCon n _) = Text.unpack n
    show (TVar (TV n _)) = Text.unpack n
    show (a :@: b) = show a ++ ' ' : show b
