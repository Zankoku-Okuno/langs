{-#LANGUAGE PolyKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module F.Scope () where

import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Writer

import F.Syntax


instance (Eq (id TermLevel), Eq (id TypeLevel)) => Substitute (Gamma attr c id Term Type Nada) (Term attr c id) where
    subst ctx0 e = runReader (goTerm e) ctx0
goTerm e@(Var' x) = fromMaybe e <$> asks (lookup x . terms)
goTerm c@(Const' _) = pure c
goTerm (Abs attr (x, t) e) = do
    t' <- goType t
    e' <- local (delTerm x) $ goTerm e
    pure $ Abs attr (x, t') e'
goTerm (App attr e1 e2) = App attr <$> goTerm e1 <*> goTerm e2
goTerm (BigAbs attr a e) = do
    e' <- local (delType a) $ goTerm e
    pure $ BigAbs attr a e'
goTerm (BigApp attr e t) = BigApp attr <$> goTerm e <*> goType t


instance (Eq (id TypeLevel)) => Substitute (Gamma attr c id Term Type Nada) (Type attr c id) where
    subst ctx0 t = runReader (goType t) ctx0
goType t@(Var' a) = fromMaybe t <$> asks (lookup a . types)
goType (Ctor attr c ts) = Ctor attr c <$> (goType `mapM` ts)
goType (Forall attr a t) = do
    t' <- local (delType a) $ goType t
    pure $ Forall attr a t'