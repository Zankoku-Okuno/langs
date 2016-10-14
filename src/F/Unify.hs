module F.Unify where

import F.Syntax
import F.Scope


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