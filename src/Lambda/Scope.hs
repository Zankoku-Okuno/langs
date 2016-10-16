{-#LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, PolyKinds #-}
module Lambda.Scope where

import Control.Monad.Reader
import Control.Monad.Writer

import Context
import Scope
import Lambda.Syntax


instance (Eq (id TermLevel)) => Substitute (Gamma attr c id Term Nada Nada) (Term attr c id) where
    subst ctx (Var attr x) = case ctx `getTerm` x of
        Nothing -> Var attr x
        Just e -> e
    subst ctx c@(Const' _) = c
    subst ctx (Abs attr x e) = Abs attr x $ subst (delTerm x ctx) e
    subst ctx (App attr e1 e2) = App attr (subst ctx e1) (subst ctx e2)


    
--FIXME use the Gamma stuff from Identifier
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