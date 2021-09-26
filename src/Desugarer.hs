{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}
{-# Language LambdaCase #-}

module Desugarer (desugar) where

import qualified Syntax as S
import qualified Core as C
import Base.Pattern
import Base.Literal

import Monad
import Syntax
import Core

desugar :: S.Module -> Astral C.UntypedModule
desugar (name, decls) = do
    decls' <- traverse desugarDecl decls
    pure (name, decls')

desugarDecl :: S.Decl -> Astral C.UntypedDecl
desugarDecl = \case
    S.DLetFn name sig defs -> do
        defs' <- traverse (\(pats, expr) -> (pats,) <$> desugarExpr expr) defs
        pure (C.DLet name sig defs')
    S.DLet name sig expr -> do
        expr' <- desugarExpr expr
        pure (C.DLet name sig [([], expr')])
    S.DOper opdef -> pure (C.DOper opdef)

desugarExpr :: S.Expr -> Astral C.UntypedExpr
desugarExpr = \case
    S.EVar name -> pure (C.EVar () name)
    S.ELit lit -> pure (C.ELit () lit)
    S.EApp a b -> do
        a' <- desugarExpr a
        b' <- desugarExpr b
        pure (C.EApp () a' b')
    S.ELam ptrns expr -> do
        let lambdas = map desugarLambdaPattern ptrns
        expr' <- desugarExpr expr
        pure (foldr ($) expr' lambdas)
    S.ELet name bind expr -> do
        bind' <- desugarExpr bind
        expr' <- desugarExpr expr
        pure (C.ELet () name bind' expr')
    S.EMatch expr branches -> do
        expr' <- desugarExpr expr
        branches' <- traverse (\(pat, expr) -> (pat,) <$> desugarExpr expr) branches
        pure (C.EMatch () expr' branches')
    S.EIf c a b -> do
        c' <- desugarExpr c
        a' <- desugarExpr a
        b' <- desugarExpr b
        pure (C.EIf () c' a' b')
    S.EBinOp op l r -> do
        l' <- desugarExpr l
        r' <- desugarExpr r
        pure (C.EApp () (C.EApp () (C.EVar () op) l') r')
    S.EUnaOp op a -> do
        a' <- desugarExpr a
        pure (C.EApp () (C.EVar () op) a')
    S.EList list -> do
        list' <- traverse desugarExpr list
        pure (C.ELit () LUnit) -- TODO
    S.EString string -> do
        pure (C.ELit () LUnit) -- TODO

desugarLambdaPattern :: Pattern -> (C.UntypedExpr -> C.UntypedExpr)
desugarLambdaPattern (PVar n) = C.ELam () n
desugarLambdaPattern ptrn = \expr -> C.ELam () "_a" (C.EMatch () (C.EVar () "_a") [(ptrn, expr)])
