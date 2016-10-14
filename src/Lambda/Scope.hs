{-#LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Lambda.Scope where

import Control.Monad.Reader
import Control.Monad.Writer

import Lambda.Syntax


type Context id = [id TermLevel]
type Subst attr c id = [(id TermLevel, Term attr c id)]

type ScopeError attr id = (attr, id TermLevel)
scopeCheck :: (Eq (id TermLevel)) => Term attr c id -> [ScopeError attr id]
scopeCheck e = execWriter (runReaderT (go e) [])
    where
    go (Var attr x) = do
        isDefd <- asks (x `elem`)
        if isDefd then pure () else tell [(attr, x)]
    go (Const' _) = pure ()
    go (Abs' x e) = local (x:) (go e)
    go (App' e1 e2) = go e1 >> go e2 >> pure ()

subst :: (Eq (id TermLevel)) => Subst attr c id -> Term attr c id -> Term attr c id
subst ctx (Var attr x) = case lookup x ctx of
    Nothing -> Var attr x
    Just e -> e
subst ctx c@(Const' _) = c
subst ctx (Abs attr x e) = Abs attr x $ subst (filter ((/= x) . fst) ctx) e
subst ctx (App attr e1 e2) = App attr (subst ctx e1) (subst ctx e2)
