{-#LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns,
            FlexibleInstances, FlexibleContexts,
            MultiParamTypeClasses #-}
module F.Syntax
    ( Type, Term
    , module Identifier
    , module Syntax, ArrC
    ) where

import Const
import Identifier
import Syntax



data Type attr op id
    = TVar_ attr (id TypeLevel)
    | TCtor_ attr op [Type attr op id]
    | Forall_ attr (id TypeLevel) (Type attr op id)

instance VarC attr (Type attr op id) (id TypeLevel) where
    toVar = TVar_
    fromVar (TVar_ attr a) = Just (attr, a)
    fromVar _ = Nothing
instance CtorC attr (Type attr op id) op (Type attr op id) where
    toCtor = TCtor_
    fromCtor (TCtor_ attr op ts) = Just (attr, op, ts)
    fromCtor _ = Nothing
instance ForallC attr (Type attr op id) (id TypeLevel) where
    toForall = Forall_
    fromForall (Forall_ attr a sigma) = Just (attr, a, sigma)
    fromForall _ = Nothing


instance MonotypeC (Type attr op id) (Type attr op id) where
    toMonotype t@(Var' _) = Just t
    toMonotype t@(Ctor' op sigmas) = const t <$> mapM toMonotype sigmas
    toMonotype _ = Nothing
    fromMonotype = id


--instance Bind (Type op) where
--    scopeCheck (TVar sid) = identFor sid >>= \case
--        Nothing -> do
--            scopeError sid
--            pure $ TVar undefined
--        Just id -> pure $ TVar id
--    scopeCheck (TCon c ts) = TCon c <$> scopeCheck `mapM` ts
--    scopeCheck (Univ sid t) = withFresh sid $ \id ->
--        Univ id <$> scopeCheck t

--    fv (TVar a) = [a]
--    fv (TCon _ ts) = foldr union [] $ fv <$> ts
--    fv (Univ id t) = delete id $ fv t

--    type Subst (Type op) id = [(id, Type op id)]
--    subst theta (TVar a) = case lookup a theta of
--        Nothing -> TVar a
--        Just t -> t
--    subst theta (TCon c ts) = TCon c $ subst theta <$> ts
--    subst theta (Univ a t) = Univ a (subst theta' t)
--        where theta' = filter ((/= a) . fst) theta





data Term attr c op id
    = Var_ attr (id TermLevel)
    | Const_ attr c
    | Abs_ attr (id TermLevel, Type attr op id) (Term attr c op id)
    | App_ attr (Term attr c op id) (Term attr c op id)
    | BigAbs_ attr (id TypeLevel) (Term attr c op id)
    | BigApp_ attr (Term attr c op id) (Type attr op id)

instance VarC attr (Term attr c op id) (id TermLevel) where
    toVar = Var_
    fromVar (Var_ attr x) = Just (attr, x)
    fromVar _ = Nothing
instance ConstC attr (Term attr c op id) c where
    toConst = Const_
    fromConst (Const_ attr c) = Just (attr, c)
    fromConst _ = Nothing
instance AbsC attr (Term attr c op id) (id TermLevel, Type attr op id) where
    toAbs = Abs_
    fromAbs (Abs_ attr x e) = Just (attr, x, e)
    fromAbs _ = Nothing
instance AppC attr (Term attr c op id) where
    toApp = App_
    fromApp (App_ attr e1 e2) = Just (attr, e1, e2)
    fromApp _ = Nothing
instance BigAbsC attr (Term attr c op id) (id TypeLevel) where
    toBigAbs = BigAbs_
    fromBigAbs (BigAbs_ attr a e) = Just (attr, a, e)
    fromBigAbs _ = Nothing
instance BigAppC attr (Term attr c op id) (Type attr op id) where
    toBigApp = BigApp_
    fromBigApp (BigApp_ attr e t) = Just (attr, e, t)
    fromBigApp _ = Nothing



--instance Bind (Term c op) where
--    scopeCheck (Var sid) = identFor sid >>= \case
--        Nothing -> do
--            scopeError sid
--            pure $ Var undefined
--        Just id -> pure $ Var id
--    scopeCheck (Con c) = pure (Con c)
--    scopeCheck (Abs (sid, t) e) = do
--        t' <- scopeCheck t
--        withFresh sid $ \x -> do
--            e' <- scopeCheck e
--            pure $ Abs (x, t') e'
--    scopeCheck (App e1 e2) = App <$> scopeCheck e1 <*> scopeCheck e2
--    scopeCheck (TyAbs sid e) = withFresh sid $ \a -> do
--        e' <- scopeCheck e
--        pure $ TyAbs a e'
--    scopeCheck (TyApp e t) = TyApp <$> scopeCheck e <*> scopeCheck t

--    fv (Var x) = [x]
--    fv (Con _) = []
--    fv (Abs (x, _) e) = delete x $ fv e
--    fv (App e1 e2) = fv e1 `union` fv e2
--    fv (TyAbs a e) = delete a $ fv e
--    fv (TyApp e t) = fv e `union` fv t

--    type Subst (Term c op) id = ([(id, Term c op id)], [(id, Type op id)])
--    subst theta (Var x) = case lookup x (fst theta) of
--        Nothing -> Var x
--        Just e -> e
--    subst theta (Con c) = Con c
--    subst theta (Abs (x, t) e) = Abs (x, subst (snd theta) t) (subst theta' e)
--        where theta' = (filter ((/= x) . fst) $ fst theta, snd theta)
--    subst theta (App e1 e2) = App (subst theta e1) (subst theta e2)
--    subst theta (TyAbs a e) = TyAbs a (subst theta' e)
--        where theta' = (fst theta, filter ((/= a) . fst) $ snd theta)
--    subst theta (TyApp e t) = TyApp (subst theta e) (subst (snd theta) t)

--ftv :: (Eq id) => Term c op id -> [id]
--ftv (Var _) = []
--ftv (Con _) = []
--ftv (Abs (_, t) e) = fv t `union` ftv e
--ftv (App e1 e2) = ftv e1 `union` ftv e2





