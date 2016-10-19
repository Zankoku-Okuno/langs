{-#LANGUAGE PolyKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module F.Scope where

import Data.Maybe
import Control.Monad.Reader

import Context
import Scope
import F.Syntax


instance (Eq (id TermLevel), Eq (id TypeLevel)) => Substitute (Gamma attr c id Term Type (Paramd Int)) (Term attr c id) where
    subst ctx0 e = runReader (goTerm e) ctx0
goTerm e@(Var' x) = fromMaybe e <$> asks (`getTerm` x)
goTerm c@(Const' _) = pure c
goTerm (Abs attr (x, t) e) = do
    t' <- goType t
    e' <- local (delTerm x) $ goTerm e
    pure $ Abs attr (x, t') e'
goTerm (App attr e1 e2) = App attr <$> goTerm e1 <*> goTerm e2
goTerm (BigAbs attr (a, k) e) = do
    k' <- goKind k
    e' <- local (delType a) $ goTerm e
    pure $ BigAbs attr (a, k') e'
goTerm (BigApp attr e t) = BigApp attr <$> goTerm e <*> goType t


instance (Eq (id TypeLevel)) => Substitute (Gamma attr c id Term Type (Paramd Int)) (Type attr c id) where
    subst ctx0 t = runReader (goType t) ctx0
goType t@(Var' a) = fromMaybe t <$> asks (`getType` a)
goType c@(Const' _) = pure c
goType (App attr t1 t2) = App attr <$> goType t1 <*> goType t2
goType (Forall attr (a, k) t) = do
    k' <- goKind k
    t' <- local (delType a) $ goType t
    pure $ Forall attr (a, k') t'


instance Substitute (Gamma attr c id Term Type (Paramd Int)) (Kind attr c id) where
    subst ctx0 k = runReader (goKind k) ctx0
goKind k = pure k