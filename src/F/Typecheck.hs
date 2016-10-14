{-#LANGUAGE FlexibleContexts #-}
module F.Typecheck where

import Control.Monad.Reader

import F.Syntax
import F.Scope hiding (Context(..))

data Context attr c op id = Ctx
    { terms :: [(id TermLevel, Type attr op id)]
    , consts :: [(c, Type attr op id)]
    }


typeCheck :: (Eq (id TermLevel), Eq (id TypeLevel),
                Eq c, Eq op, ArrC op) =>
             Context attr c op id -> Term attr c op id -> Maybe (Type attr op id)
typeCheck ctx0 e = runReaderT (go e) ctx0
    where
    go (Var' x) = lift . lookup x =<< asks terms
    go (Const' c) = lift . lookup c =<< asks consts
    go (Abs' (x, t1) e) = do
        ctx <- ask
        let ctx' = ctx { terms = (x, t1) : terms ctx }
        t2 <- local (const ctx') (go e)
        pure $ Arr undefined t1 t2
    go (App' e1 e2) = do
        tf <- go e1
        t <- go e2
        lift $ unifyFun tf t
    go (BigAbs' a e) = Forall undefined a <$> go e
    go (BigApp' e t) = do
        sigma <- go e
        lift $ inst sigma t




------------ FIXME move to a unify module


-- NOTE: this is not equivalence, but mere syntactic equality, not even up to alpha-renaming
instance (Eq op, Eq (id TypeLevel)) => Eq (Type attr op id) where
    (Var' x1) == (Var' x2) = x1 == x2
    (Ctor' c1 args1) == (Ctor' c2 args2) = c1 == c2 && args1 == args2
    (Forall' _ _) == _ = error "Attempt to test equality on polytype; did you mean equivalence?"
    _ == (Forall' _ _) = error "Attempt to test equality on polytype; did you mean equivalence?"
    _ == _ = False

tyEquiv :: (Eq (id TypeLevel), Eq op) =>
            Type attr op id -> Type attr op id -> Bool
tyEquiv (Mono t1) (Mono t2) = t1 == t2
tyEquiv (Forall' a1 t1) (Forall' a2 t2) =
    let theta = Subst [] [(a1, Var undefined a2)]
    in t1 == tySubst theta t2


unifyFun :: (Eq (id TermLevel), Eq (id TypeLevel), Eq op,
                ArrC op) =>
            Type attr op id -> Type attr op id -> Maybe (Type attr op id)
unifyFun (Arr' t1 t2) t1' | t1 `tyEquiv` t1' = Just t2
unifyFun _ _ = Nothing

inst :: (Eq (id TypeLevel), Eq op) => Type attr op id -> Type attr op id -> Maybe (Type attr op id)
inst (Forall' a sigma) t = Just $
    let theta = Subst [] [(a, t)]
    in tySubst theta sigma
inst _ _ = Nothing