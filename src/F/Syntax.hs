{-#LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns,
            FlexibleInstances, FlexibleContexts,
            MultiParamTypeClasses #-}
module F.Syntax
    ( Type, Term
    , module Identifier
    , module Syntax
    ) where

import Identifier
import Syntax



data Type attr c id
    = TVar_ attr (id TypeLevel)
    | TCtor_ attr (c TypeLevel) [Type attr c id]
    | Forall_ attr (id TypeLevel) (Type attr c id)

instance VarC attr (Type attr c id) (id TypeLevel) where
    toVar = TVar_
    fromVar (TVar_ attr a) = Just (attr, a)
    fromVar _ = Nothing
instance CtorC attr (Type attr c id) (c TypeLevel) (Type attr c id) where
    toCtor = TCtor_
    fromCtor (TCtor_ attr c ts) = Just (attr, c, ts)
    fromCtor _ = Nothing
instance ForallC attr (Type attr c id) (id TypeLevel) where
    toForall = Forall_
    fromForall (Forall_ attr a sigma) = Just (attr, a, sigma)
    fromForall _ = Nothing


instance MonotypeC (Type attr c id) (Type attr c id) where
    toMonotype t@(Var' _) = Just t
    toMonotype t@(Ctor' c sigmas) = const t <$> mapM toMonotype sigmas
    toMonotype _ = Nothing
    fromMonotype = id




data Term attr c id
    = Var_ attr (id TermLevel)
    | Const_ attr (c TermLevel)
    | Abs_ attr (id TermLevel, Type attr c id) (Term attr c id)
    | App_ attr (Term attr c id) (Term attr c id)
    | BigAbs_ attr (id TypeLevel) (Term attr c id)
    | BigApp_ attr (Term attr c id) (Type attr c id)

instance VarC attr (Term attr c id) (id TermLevel) where
    toVar = Var_
    fromVar (Var_ attr x) = Just (attr, x)
    fromVar _ = Nothing
instance ConstC attr (Term attr c id) (c TermLevel) where
    toConst = Const_
    fromConst (Const_ attr c) = Just (attr, c)
    fromConst _ = Nothing
instance AbsC attr (Term attr c id) (id TermLevel, Type attr c id) where
    toAbs = Abs_
    fromAbs (Abs_ attr x e) = Just (attr, x, e)
    fromAbs _ = Nothing
instance AppC attr (Term attr c id) where
    toApp = App_
    fromApp (App_ attr e1 e2) = Just (attr, e1, e2)
    fromApp _ = Nothing
instance BigAbsC attr (Term attr c id) (id TypeLevel) where
    toBigAbs = BigAbs_
    fromBigAbs (BigAbs_ attr a e) = Just (attr, a, e)
    fromBigAbs _ = Nothing
instance BigAppC attr (Term attr c id) (Type attr c id) where
    toBigApp = BigApp_
    fromBigApp (BigApp_ attr e t) = Just (attr, e, t)
    fromBigApp _ = Nothing
