{-#LANGUAGE LambdaCase,
            PatternSynonyms, ViewPatterns,
            TypeFamilies,
            LiberalTypeSynonyms #-}
module F.Syntax where

import Data.List

import Control.Monad.Reader
import Control.Monad.Writer

import AList hiding (lookup)
import qualified AList as A
import Ident
import Const

data Sid = TermId String
         | TypeId String
    deriving (Eq)

data Id = Id Sid Unique -- FIXME needs source location reporting
instance Show Id where
    show (Id var _) = show var

instance Eq Id where
    (Id _ x) == (Id _ y) = x == y

instance MkId Id where
    type Args Id = Sid
    mkId = Id

    type SourceId Id = Sid
    fromSource = Id





data Type tc id
    = TVar id
    | TCon tc [Type tc id]
    | Univ id (Type tc id)

pattern ArrTy a b <- (TCon (isArr -> True) [a, b])
    where
    ArrTy a b = TCon mkArr [a, b]

instance (Eq tc, Eq id) => Eq (Type tc id) where
    (TVar x1) == (TVar x2) = x1 == x2
    (TCon c1 args1) == (TCon c2 args2) = c1 == c2 && args1 == args2
    (Univ x1 t1) == (Univ x2 t2) = t1 == subst [(x2, TVar x1)] t2
    _ == _ = False

instance Bind (Type tc) where
    scopeCheck (TVar sid) = identFor sid >>= \case
        Nothing -> do
            scopeError sid
            pure $ TVar undefined
        Just id -> pure $ TVar id
    scopeCheck (TCon c ts) = TCon c <$> scopeCheck `mapM` ts
    scopeCheck (Univ sid t) = withFresh sid $ \id ->
        Univ id <$> scopeCheck t

    fv (TVar a) = [a]
    fv (TCon _ ts) = foldr union [] $ fv <$> ts
    fv (Univ id t) = delete id $ fv t

    type Subst (Type tc) id = [(id, Type tc id)]
    subst theta (TVar a) = case lookup a theta of
        Nothing -> TVar a
        Just t -> t
    subst theta (TCon c ts) = TCon c $ subst theta <$> ts
    subst theta (Univ a t) = Univ a (subst theta' t)
        where theta' = filter ((/= a) . fst) theta





data Term c tc id
    = Var id
    | Con c
    | Abs (id, Type tc id) (Term c tc id)
    | App (Term c tc id) (Term c tc id)
    | TyAbs id (Term c tc id)
    | TyApp (Term c tc id) (Type tc id)


instance Bind (Term c tc) where
    scopeCheck (Var sid) = identFor sid >>= \case
        Nothing -> do
            scopeError sid
            pure $ Var undefined
        Just id -> pure $ Var id
    scopeCheck (Con c) = pure (Con c)
    scopeCheck (Abs (sid, t) e) = do
        t' <- scopeCheck t
        withFresh sid $ \x -> do
            e' <- scopeCheck e
            pure $ Abs (x, t') e'
    scopeCheck (App e1 e2) = App <$> scopeCheck e1 <*> scopeCheck e2
    scopeCheck (TyAbs sid e) = withFresh sid $ \a -> do
        e' <- scopeCheck e
        pure $ TyAbs a e'
    scopeCheck (TyApp e t) = TyApp <$> scopeCheck e <*> scopeCheck t

    fv (Var x) = [x]
    fv (Con _) = []
    fv (Abs (x, _) e) = delete x $ fv e
    fv (App e1 e2) = fv e1 `union` fv e2
    fv (TyAbs a e) = delete a $ fv e
    fv (TyApp e t) = fv e `union` fv t

    type Subst (Term c tc) id = ([(id, Term c tc id)], [(id, Type tc id)])
    subst theta (Var x) = case lookup x (fst theta) of
        Nothing -> Var x
        Just e -> e
    subst theta (Con c) = Con c
    subst theta (Abs (x, t) e) = Abs (x, subst (snd theta) t) (subst theta' e)
        where theta' = (filter ((/= x) . fst) $ fst theta, snd theta)
    subst theta (App e1 e2) = App (subst theta e1) (subst theta e2)
    subst theta (TyAbs a e) = TyAbs a (subst theta' e)
        where theta' = (fst theta, filter ((/= a) . fst) $ snd theta)
    subst theta (TyApp e t) = TyApp (subst theta e) (subst (snd theta) t)

ftv :: (Eq id) => Term c tc id -> [id]
ftv (Var _) = []
ftv (Con _) = []
ftv (Abs (_, t) e) = fv t `union` ftv e
ftv (App e1 e2) = ftv e1 `union` ftv e2






instance Show Sid where
    show (TermId x) = x
    show (TypeId a) = '\'':a
instance (Show tc, Show id, Arr tc) => Show (Type tc id) where
    show (TVar a) = show a
    show (ArrTy a b) = concat ["(", show a, " -> ", show b, ")"]
    show (TCon c ts) = "(" ++ intercalate " " (show c : (show <$> ts)) ++ ")"
    show (Univ a t) = concat ["(∀", show a, ". ", show t, ")"]
instance (Show tc, Show id, Arr tc) => Show (Term c tc id) where
    show (Var x) = show x
    show (Abs (x, t) e) = concat ["(", "λ", show x, ". ", show e, ")"]
    show (App e1 e2) = concat ["(", show e1, " ", show e2, ")"]
    show (TyAbs a e) = concat ["(", "Λ", show a, ". ", show e, ")"]
    show (TyApp e t) = concat [show e, "[", show t, "]"]

