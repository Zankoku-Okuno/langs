{-#LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Lambda.Syntax
    ( Term
    , module Identifier
    , module Syntax
    ) where

import Syntax
import Identifier


data Term attr c id
    = Var_ attr (id TermLevel)
    | Const_ attr (c TermLevel)
    | Abs_ attr (id TermLevel) (Term attr c id)
    | App_ attr (Term attr c id) (Term attr c id)


instance VarC attr (Term attr c id) (id TermLevel) where
    toVar = Var_
    fromVar (Var_ attr x) = Just (attr, x)
    fromVar _ = Nothing
instance ConstC attr (Term attr c id) (c TermLevel) where
    toConst = Const_
    fromConst (Const_ attr c) = Just (attr, c)
    fromConst _ = Nothing
instance AbsC attr (Term attr c id) (id TermLevel) where
    toAbs = Abs_
    fromAbs (Abs_ attr x e) = Just (attr, x, e)
    fromAbs _ = Nothing
instance AppC attr (Term attr c id) where
    toApp = App_
    fromApp (App_ attr e1 e2) = Just (attr, e1, e2)
    fromApp _ = Nothing


instance ValueC (Term attr c id) (Term attr c id) where
    toValue x@(Const' _) = Just x
    toValue x@(Abs' _ _) = Just x
    toValue _ = Nothing
    fromValue = id
