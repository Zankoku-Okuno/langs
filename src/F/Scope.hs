{-#LANGUAGE KindSignatures, FlexibleContexts #-}
module F.Scope where

import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Writer

import F.Syntax


type Subst attr c id = Gamma attr c id Term Type Nada

subst :: (Eq (id TermLevel), Eq (id TypeLevel)) =>
         Subst attr c id -> Term attr c id -> Term attr c id
subst ctx0 e = runReader (goTerm e) ctx0

tySubst :: (Eq (id TypeLevel)) =>
            Subst attr c id -> Type attr c id -> Type attr c id
tySubst ctx0 e = runReader (goType e) ctx0

goTerm e@(Var' x) = fromMaybe e <$> asks (lookup x . termGamma)
goTerm c@(Const' _) = pure c
goTerm (Abs attr (x, t) e) = do
    t' <- goType t
    e' <- local (delTermGamma x) $ goTerm e
    pure $ Abs attr (x, t') e'
goTerm (App attr e1 e2) = App attr <$> goTerm e1 <*> goTerm e2
goTerm (BigAbs attr a e) = do
    e' <- local (delTypeGamma a) $ goTerm e
    pure $ BigAbs attr a e'
goTerm (BigApp attr e t) = BigApp attr <$> goTerm e <*> goType t

goType t@(Var' a) = fromMaybe t <$> asks (lookup a . typeGamma)
goType (Ctor attr c ts) = Ctor attr c <$> (goType `mapM` ts)
goType (Forall attr a t) = do
    t' <- local (delTypeGamma a) $ goType t
    pure $ Forall attr a t'