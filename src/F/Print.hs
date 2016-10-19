{-#LANGUAGE PolyKinds, FlexibleContexts, UndecidableInstances #-}
module F.Print () where

import Data.List (intercalate)

import F.Syntax


instance ( Show (id TermLevel), Show (id TypeLevel)
         , Show (c TermLevel), Show (c TypeLevel), Show (c KindLevel)
         , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
         ) => Show (Term attr c id) where
    show = disp UTop
instance (Show (id TypeLevel), Show (c TypeLevel), Show (c KindLevel)
         , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
         ) => Show (Type attr c id) where
    show = dispT UTop
instance ( Show (c KindLevel)
         , ArrC attr (Kind attr c id)
         ) => Show (Kind attr c id) where
    show = dispK UTop

data Under = UTop | UAbs | UApp | UArrL | UArrR


disp :: ( Show (id TermLevel), Show (id TypeLevel)
        , Show (c TermLevel), Show (c TypeLevel), Show (c KindLevel)
        , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
        ) => Under -> Term attr c id -> String
disp _ (Var' x) = show x
disp _ (Const' x) = show x
disp UApp e@(Abs' _ _) = inParens $ disp UTop e
disp _ (Abs' (x, t@(Forall' _ _)) e) = concat ["λ(", show x, " : ", dispT UTop t, "). ", disp UAbs e]
disp _ (Abs' (x, t) e) = concat ["λ", show x, " : ", dispT UTop t, ". ", disp UAbs e]
disp _ (App' e1 e2) = concat [disp UApp e1, " ", disp UApp e2]
disp UApp e@(BigAbs' _ _) = inParens $ disp UTop e
disp _ (BigAbs' (a, k) e) = concat ["Λ", show a, ":", dispK UTop k, ". ", disp UAbs e]
disp _ (BigApp' e t) = concat [disp UApp e, "[", dispT UTop t, "]"]


dispT :: ( Show (id TypeLevel), Show (c TypeLevel), Show (c KindLevel)
         , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
         ) => Under -> Type attr c id -> String
dispT _ (Var' a) = show a
dispT _ (Const' c) = show c
dispT UArrL t@(Arr' _ _) = inParens $ dispT UTop t
dispT _ (Arr' a b) = concat [dispT UArrL a, " -> ", dispT UArrR b]
dispT _ (App' t1 t2) = concat [dispT UApp t1, " ", dispT UApp t2]
dispT UArrL t@(Forall' _ _) = inParens $ dispT UTop t
dispT UArrR t@(Forall' _ _) = inParens $ dispT UTop t
dispT UApp t@(Forall' _ _) = inParens $ dispT UTop t
dispT _ (Forall' (a, k) t) = concat ["∀", show a, ":", dispK UTop k, ". ", dispT UAbs t]


dispK :: (Show (c KindLevel), ArrC attr (Kind attr c id)
         ) => Under -> Kind attr c id -> String
dispK UArrL k@(Arr' _ _) = inParens $ dispK UTop k
dispK _ k@(Arr' a b) = concat [dispK UArrL a, " -> ", dispK UArrL b]
dispK _ (Ctor' c ks) = intercalate " " $ show c : (dispK UApp <$> ks)

inParens str = "(" ++ str ++ ")"