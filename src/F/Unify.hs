{-#LANGUAGE FlexibleContexts, FlexibleInstances #-}
module F.Unify where

import Scope
import F.Syntax
import F.Scope


-- WARNING: this is not equivalence, but mere syntactic equality, not even up to alpha-renaming
instance ( Eq (id TypeLevel), Eq (c TypeLevel)
         ) => Eq (Type attr c id) where
    (Var' a1) == (Var' a2) = a1 == a2
    (Const' c1) == (Const' c2) = c1 == c2
    (App' t1 t1') == (App' t2 t2') = t1 == t2 && t1' == t2'
    (Forall' _ _) == _ = error "Attempt to test equality on polytype; did you mean equivalence?"
    _ == (Forall' _ _) = error "Attempt to test equality on polytype; did you mean equivalence?"
    _ == _ = False

instance ( Eq (c KindLevel)
         ) => Eq (Kind attr c id) where
    (Ctor' c1 ks1) == (Ctor' c2 ks2) = c1 == c2 && ks1 == ks2


tyEquiv :: ( Eq (id TypeLevel), Eq (c TypeLevel), Eq (c KindLevel)
           ) => Type attr c id -> Type attr c id -> Bool
tyEquiv (Var' a) (Var' b) = a == b
tyEquiv (Const' c1) (Const' c2) = c1 == c2
tyEquiv (App' t1 t1') (App' t2 t2') =
    t1 `tyEquiv` t2
    && t1' `tyEquiv` t2'
tyEquiv (Forall' (a, k1) t1) (Forall' (b, k2) t2) =
    k1 == k2
    && t1 `tyEquiv` ((b, Var undefined a) `substType` t2)
tyEquiv _ _ = False


unifyFun :: ( Eq (id TermLevel), Eq (id TypeLevel)
            , Eq (c TypeLevel), Eq (c KindLevel)
            , ArrC attr (Type attr c id)
            ) => Type attr c id -> Type attr c id -> Maybe (Type attr c id)
unifyFun (Arr' t1 t2) t1' | t1 `tyEquiv` t1' = Just t2
unifyFun _ _ = Nothing

unifyTypeFun :: ( Eq (c KindLevel), ArrC attr (Kind attr c id)
                ) => Kind attr c id -> Kind attr c id -> Maybe (Kind attr c id)
unifyTypeFun (Arr' k1 k2) k1' | k1 == k1' = Just k2
unifyTypeFun _ _ = Nothing

inst :: (Eq (id TypeLevel), Eq (c TypeLevel), Eq (c KindLevel)
        ) => Type attr c id -> (Type attr c id, Kind attr c id) -> Maybe (Type attr c id)
inst (Forall' (a, k) sigma) (t, k') | k == k' = Just $ (a, t) `substType` sigma
inst _ _ = Nothing