{-#LANGUAGE PolyKinds, FlexibleInstances, MultiParamTypeClasses #-}
module FOmegaNoLambda.Syntax
    ( Term, Type, Kind
    , module Identifier
    , module Syntax
    ) where

import Identifier
import Syntax


data Kind attr c id
    = KCtor_ attr (c KindLevel) [Kind attr c id]

instance CtorC attr (Kind attr c id) (c KindLevel) (Kind attr c id) where
    toCtor = KCtor_
    fromCtor (KCtor_ attr c ks) = Just (attr, c, ks)


data Type attr c id
    = TVar_ attr (id TypeLevel)
    | TConst_ attr (c TypeLevel)
    | TApp_ attr (Type attr c id) (Type attr c id)
    | TForall_ attr (id TypeLevel, Kind attr c id) (Type attr c id)

instance VarC attr (Type attr c id) (id TypeLevel) where
    toVar = TVar_
    fromVar (TVar_ attr a) = Just (attr, a)
    fromVar _ = Nothing
instance ConstC attr (Type attr c id) (c TypeLevel) where
    toConst = TConst_
    fromConst (TConst_ attr a) = Just (attr, a)
    fromConst _ = Nothing
instance AppC attr (Type attr c id) where
    toApp = TApp_
    fromApp (TApp_ attr t1 t2) = Just (attr, t1, t2)
    fromApp _ = Nothing
instance ForallC attr (Type attr c id) (id TypeLevel, Kind attr c id) where
    toForall = TForall_
    fromForall (TForall_ attr a t) = Just (attr, a, t)
    fromForall _ = Nothing

instance MonotypeC (Type attr c id) (Type attr c id) where
    toMonotype t@(Var' _) = Just t
    toMonotype t@(Const' _) = Just t
    toMonotype t@(App' t1 t2) = const t <$> mapM_ toMonotype [t1, t2]
    toMonotype _ = Nothing
    fromMonotype = id


data Term attr c id
    = Var_ attr (id TermLevel)
    | Const_ attr (c TermLevel)
    | Abs_ attr (id TermLevel, Type attr c id) (Term attr c id)
    | App_ attr (Term attr c id) (Term attr c id)
    | BigAbs_ attr (id TypeLevel, Kind attr c id) (Term attr c id)
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
instance BigAbsC attr (Term attr c id) (id TypeLevel, Kind attr c id) where
    toBigAbs = BigAbs_
    fromBigAbs (BigAbs_ attr a e) = Just (attr, a, e)
    fromBigAbs _ = Nothing
instance BigAppC attr (Term attr c id) (Type attr c id) where
    toBigApp = BigApp_
    fromBigApp (BigApp_ attr e t) = Just (attr, e, t)
    fromBigApp _ = Nothing
