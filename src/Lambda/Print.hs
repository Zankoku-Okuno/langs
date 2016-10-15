{-#LANGUAGE GADTs #-}
module Lambda.Print () where

import Lambda.Syntax

-- FIXME replace the show instance by just a function that displays ids
instance ( Show (id TermLevel), Show (c TermLevel)
         ) => Show (Term attr c id) where
    show = disp UTop

data Under = UTop | UAbs | UApp

disp :: ( Show (id TermLevel), Show (c TermLevel)
        ) => Under -> Term attr c id -> String
disp _        (Var' x)     = show x
disp _        (Const' c)   = show c
disp UApp abs@(Abs' _ _)   = inParens $ disp UTop abs
disp _        (Abs' x e)   = concat ["λ", show x, ". ", disp UAbs e]
disp _        (App' e1 e2) = concat [disp UApp e1, " ", disp UApp e2]




inParens str = concat ["(", str, ")"]

