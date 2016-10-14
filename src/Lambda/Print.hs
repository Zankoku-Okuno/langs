{-#LANGUAGE GADTs #-}
module Lambda.Print () where

import Lambda.Syntax


instance Show (SourceId level) where
    show (SourceTermId x) = x
    show (SourceTypeId a) = '\'':a
    show (SourceKindId k) = "''"++k
instance (Show (id TermLevel), Show c) => Show (Term attr c id) where
    show = disp UTop

data Under = UTop | UAbs | UFun | UArg

disp :: (Show (id TermLevel), Show c) => Under -> Term attr c id -> String
disp _        (Var' x)     = show x
disp _        (Const' c)   = show c
disp UFun abs@(Abs' _ _)   = inParens $ disp UTop abs
disp _        (Abs' x e)   = concat ["λ", show x, ". ", disp UAbs e]
disp UFun     (App' e1 e2) = concat [disp UFun e1, " ", disp UArg e2]
disp UArg app@(App' _ _)   = inParens $ disp UTop app
disp _        (App' e1 e2) = concat [disp UFun e1, " ", disp UArg e2]




inParens str = concat ["(", str, ")"]
