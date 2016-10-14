{-#LANGUAGE FlexibleInstances, GADTs #-}
module F.Print () where

import Data.List (intercalate)

import F.Syntax
--FIXME eliminate unnessecary parens

instance (Show (id TermLevel), Show (id TypeLevel), Show c, Show op, ArrC op) =>
         Show (Term attr c op id) where
    show = disp UTop
instance (Show (id TypeLevel), Show op, ArrC op) => Show (Type attr op id) where
    show = dispT UTop

data Under = UTop | UAbs | UFun | UArg | UArrL | UArrR



dispT :: (Show (id TypeLevel), Show op, ArrC op) =>
         Under -> Type attr op id -> String
dispT _ (Var' a) = show a
dispT UArrL t@(Arr' _ _) = inParens $ dispT UTop t
dispT _ (Arr' a b) = concat [dispT UArrL a, " -> ", dispT UArrR b]
dispT UArg t@(Ctor' _ _) = inParens $ dispT UTop t
dispT _ (Ctor' c ts) = intercalate " " $ show c : (dispT UArg <$> ts)
dispT UArrL t@(Forall' _ _) = inParens $ dispT UTop t
dispT UArrR t@(Forall' _ _) = inParens $ dispT UTop t
dispT UArg t@(Forall' _ _) = inParens $ dispT UTop t
dispT _ (Forall' a t) = concat ["∀", show a, ". ", dispT UAbs t]

disp :: (Show (id TermLevel), Show (id TypeLevel), Show c, Show op, ArrC op) =>
        Under -> Term attr c op id -> String
disp _ (Var' x) = show x
disp UFun e@(Abs' _ _) = inParens $ disp UTop e
disp _ (Abs' (x, t@(Forall' _ _)) e) = concat ["λ(", show x, " : ", dispT UTop t, "). ", disp UAbs e]
disp _ (Abs' (x, t) e) = concat ["λ", show x, " : ", dispT UTop t, ". ", disp UAbs e]
disp UArg e@(App' _ _) = inParens $ disp UTop e
disp _ (App' e1 e2) = concat [disp UFun e1, " ", disp UArg e2]
disp UFun e@(BigAbs' _ _) = inParens $ disp UTop e
disp _ (BigAbs' a e) = concat ["Λ", show a, ". ", disp UAbs e]
disp UArg e@(BigApp' _ _) = inParens $ disp UTop e
disp _ (BigApp' e t) = concat [disp UFun e, "[", dispT UTop t, "]"]



inParens str = "(" ++ str ++ ")"