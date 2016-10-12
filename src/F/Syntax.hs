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

data Sid = TermVar String
         | TypeVar String
    deriving (Eq)
instance Show Sid where
    show (TermVar x) = x
    show (TypeVar a) = '\'':a

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
    deriving (Show)

pattern ArrTy a b <- (TCon (isArr -> True) [a, b])
    where
    ArrTy a b = TCon mkArr [a, b]


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
    substs theta (TVar a) = case lookup a theta of
        Nothing -> TVar a
        Just t -> t
    substs theta (TCon c ts) = TCon c $ substs theta <$> ts
    substs theta (Univ a t) = Univ a (substs theta' t)
        where theta' = filter ((/= a) . fst) theta





data Term c tc id
    = Var id
    | Con c
    | Abs (id, Type tc id) (Term c tc id)
    | App (Term c tc id) (Term c tc id)
    | TyAbs id (Term c tc id)
    | TyApp (Term c tc id) (Type tc id)
    deriving (Show)


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
    substs theta (Var x) = case lookup x (fst theta) of
        Nothing -> Var x
        Just e -> e
    substs theta (Con c) = Con c
    substs theta (Abs (x, t) e) = Abs (x, substs (snd theta) t) (substs theta' e)
        where theta' = (filter ((/= x) . fst) $ fst theta, snd theta)
    substs theta (App e1 e2) = App (substs theta e1) (substs theta e2)
    substs theta (TyAbs a e) = TyAbs a (substs theta' e)
        where theta' = (fst theta, filter ((/= a) . fst) $ snd theta)
    substs theta (TyApp e t) = TyApp (substs theta e) (substs (snd theta) t)

ftv :: (Eq id) => Term c tc id -> [id]
ftv (Var _) = []
ftv (Con _) = []
ftv (Abs (_, t) e) = fv t `union` ftv e
ftv (App e1 e2) = ftv e1 `union` ftv e2


