{-#LANGUAGE FlexibleContexts #-}
module F.Scope where

import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Writer

import F.Syntax


data Context id = Ctx
    { terms :: [id TermLevel]
    , types :: [id TypeLevel]
    }

type ScopeError attr id = (attr, Either (id TermLevel) (id TypeLevel))
scopeCheck :: (Eq (id TermLevel), Eq (id TypeLevel)) =>
              Context id -> Term attr c op id -> [ScopeError attr id]
scopeCheck ctx0 e = execWriter (goTerm ctx0 e)
    where
    goType ctx (Var attr a)
        | a `elem` types ctx = pure ()
        | otherwise = tell [(attr, Right a)]
    goType ctx (Ctor' _ ts) = goType ctx `mapM_` ts
    goType ctx (Forall' a t) = goType (ctx { types = a:types ctx }) t
    goTerm ctx (Var attr x)
        | x `elem` terms ctx = pure ()
        | otherwise = tell [(attr, Left x)]
    goTerm ctx (Const' _) = pure ()
    goTerm ctx (Abs' (x, t) e) = do
        goType ctx t
        goTerm (ctx { terms = x:terms ctx }) e
    goTerm ctx (App' e1 e2) = goTerm ctx `mapM_` [e1, e2]
    goTerm ctx (BigAbs' a e) = goTerm (ctx { types = a:types ctx }) e
    goTerm ctx (BigApp' e t) = do
        goTerm ctx e
        goType ctx t


data Subst attr c op id = Subst
    { termTheta :: [(id TermLevel, Term attr c op id)]
    , typeTheta :: [(id TypeLevel, Type attr op id)]
    }

subst :: (Eq (id TermLevel), Eq (id TypeLevel)) =>
         Subst attr c op id -> Term attr c op id -> Term attr c op id
subst ctx0 e = runReader (goTerm e) ctx0

tySubst :: (Eq (id TypeLevel)) =>
            Subst attr c op id -> Type attr op id -> Type attr op id
tySubst ctx0 e = runReader (goType e) ctx0

goTerm e@(Var' x) = fromMaybe e <$> asks (lookup x . termTheta)
goTerm c@(Const' _) = pure c
goTerm (Abs attr (x, t) e) = do
    ctx <- ask
    let ctx' = ctx { termTheta = [th | th <- termTheta ctx, fst th /= x] }
    t' <- goType t
    e' <- local (const ctx') $ goTerm e
    pure $ Abs attr (x, t') e'
goTerm (App attr e1 e2) = App attr <$> goTerm e1 <*> goTerm e2
goTerm (BigAbs attr a e) = do
    ctx <- ask
    let ctx' = ctx { typeTheta = [th | th <- typeTheta ctx, fst th /= a] }
    e' <- local (const ctx') $ goTerm e
    pure $ BigAbs attr a e'
goTerm (BigApp attr e t) = BigApp attr <$> goTerm e <*> goType t

goType t@(Var' a) = fromMaybe t <$> asks (lookup a . typeTheta)
goType (Ctor attr c ts) = Ctor attr c <$> (goType `mapM` ts)
goType (Forall attr a t) = do
    ctx <- ask
    let ctx' = ctx { typeTheta = [th | th <- typeTheta ctx, fst th /= a] }
    t' <- local (const ctx') $ goType t
    pure $ Forall attr a t'