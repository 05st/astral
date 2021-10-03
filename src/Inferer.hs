{-# Language LambdaCase #-}

module Inferer (infer) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Except

import Base.Name
import Base.Type
import Base.Literal
import Base.Pattern

import Core
import Monad
import Substitution

data TypeError = Mismatch Type Type | Undefined Name | EmptyMatch deriving (Show)
type TEnv = Map.Map Name TypeScheme
type Infer = RWST (Map.Map Name TypeScheme) [Constraint]  InferState (Except TypeError)

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

generalize :: TEnv -> Type -> TypeScheme
generalize env t = Forall vs t
    where vs = tvs t `Set.difference` tvs (Map.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    let vs' = Set.toList vs
    nvs <- traverse (const fresh) vs'
    let s = Map.fromList (zip vs' nvs)
    pure (apply s t)

infer :: UntypedModule -> Astral TypedModule
infer = undefined

inferExpr :: UntypedExpr -> Infer (TypedExpr, Type)
inferExpr = \case
    EApp _ a b -> do
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        rt <- fresh
        constrain (CEqual at (bt :-> rt))
        pure (EApp rt a' b', rt)
    EVar _ name -> lookupType name >>= \t -> pure (EVar t name, t)
    ELam _ param expr -> do
        pt <- fresh
        (expr', rt) <- local (Map.insert param (Forall Set.empty pt)) (inferExpr expr)
        let t = pt :-> rt
        pure (ELam t param expr', t)
    EMatch _ expr branches -> do
        (expr', et) <- inferExpr expr
        (branches', bts) <- unzip <$> traverse (inferBranch et) branches
        case bts of
            [] -> throwError EmptyMatch
            (bt : rest) -> (EMatch bt expr' branches', bt) <$ mapM_ (constrain . CEqual bt) rest
    EIf _ c a b -> do
        (c', ct) <- inferExpr c
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        constrain (CEqual ct TBool)
        constrain (CEqual at bt)
        pure (EIf at c' a' b', at)
    ELet _ name bind expr -> do
        (bind', bt) <- inferExpr bind
        undefined

inferLit :: Literal -> (Literal, Type)
inferLit = \case
    LInt n -> (LInt n, TInt)
    LBool b -> (LBool b, TBool)
    LChar c -> (LChar c, TChar)

inferBranch :: Type -> (Pattern, UntypedExpr) -> Infer ((Pattern, TypedExpr), Type)
inferBranch mt (pat, expr) = do
    (pt, vars) <- inferPattern pat
    constrain (CEqual pt mt)
    (expr', et) <- local (Map.fromList vars `Map.union`) (inferExpr expr)
    pure ((pat, expr'), et)

inferPattern :: Pattern -> Infer (Type, [(Name, TypeScheme)])
inferPattern (PCon name pats) = do
    (pts, vars) <- unzip <$> traverse inferPattern pats
    undefined
inferPattern (PVar name) = do
    ptype <- fresh
    pure (ptype, [(name, Forall Set.empty ptype)])
inferPattern (PAs name pat) = do
    undefined
inferPattern (PLit lit) = pure (snd (inferLit lit), [])
inferPattern PWild = do
    ptype <- fresh
    pure (ptype, [])

lookupType :: Name -> Infer Type
lookupType name = do
    env <- ask
    case Map.lookup name env of
        Just t -> instantiate t
        Nothing -> throwError (Undefined name)
