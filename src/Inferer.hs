{-# Language LambdaCase #-}

module Inferer (infer) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Monad.Except

import Base.Name
import Base.Type

import Core
import Monad

data TypeError = Mismatch Type Type
type Infer = RWST (Map.Map Name TypeScheme) [Constraint] InferState (Except TypeError)

newtype InferState = InferState
    { count :: Int
    }

fresh :: Infer Type
fresh = do
    state <- get
    let n = count state
    put (InferState { count = n + 1 })
    pure . TVar . flip TV None $ names !! n
    where names = map (Text.pack . ('_' :)) $ [1..] >>= flip replicateM ['a'..'z']

constrain :: Constraint -> Infer ()
constrain = tell . (: [])

infer :: UntypedModule -> Astral TypedModule
infer = undefined

inferExpr :: UntypedExpr -> Infer (TypedExpr, Type)
inferExpr = \case
    EApp _ a b -> do
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        rt <- fresh
        constrain (at, bt :-> rt)
        pure (EApp rt a' b', rt)
        
