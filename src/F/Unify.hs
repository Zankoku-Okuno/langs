module F.Unify where

import F.Syntax
import F.Scope


-- NOTE: this is not equivalence, but mere syntactic equality, not even up to alpha-renaming
instance ( Eq (id TypeLevel), Eq (c TypeLevel)
         ) => Eq (Type attr c id) where
    (Var' x1) == (Var' x2) = x1 == x2
    (Ctor' c1 args1) == (Ctor' c2 args2) = c1 == c2 && args1 == args2
    (Forall' _ _) == _ = error "Attempt to test equality on polytype; did you mean equivalence?"
    _ == (Forall' _ _) = error "Attempt to test equality on polytype; did you mean equivalence?"
    _ == _ = False


tyEquiv :: ( Eq (id TypeLevel), Eq (c TypeLevel)
           ) => Type attr c id -> Type attr c id -> Bool
tyEquiv (Var' a) (Var' b) = a == b
tyEquiv (Ctor' c1 args1) (Ctor' c2 args2) =
       c1 == c2
    && and (zipWith tyEquiv args1 args2)
tyEquiv (Forall' a t1) (Forall' b t2) =
    t1 `tyEquiv` tySubst (Subst [] [(b, Var undefined a)]) t2
tyEquiv _ _ = False


unifyFun :: ( Eq (id TermLevel), Eq (id TypeLevel)
            , Eq (c TypeLevel), ArrC (c TypeLevel)
            ) => Type attr c id -> Type attr c id -> Maybe (Type attr c id)
unifyFun (Arr' t1 t2) t1' | t1 `tyEquiv` t1' = Just t2
unifyFun _ _ = Nothing

inst :: (Eq (id TypeLevel), Eq (c TypeLevel)
        ) => Type attr c id -> Type attr c id -> Maybe (Type attr c id)
inst (Forall' a sigma) t = Just $
    let theta = Subst [] [(a, t)]
    in tySubst theta sigma
inst _ _ = Nothing