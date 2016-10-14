{-#LANGUAGE PatternSynonyms, ViewPatterns,
            MultiParamTypeClasses, FunctionalDependencies #-}
module Syntax where


class VarC attr s x | s -> x, s -> attr where
    toVar :: attr -> x -> s
    fromVar :: s -> Maybe (attr, x)
pattern Var :: (VarC attr s x) => attr -> x -> s
pattern Var attr x <- (fromVar -> Just (attr, x))
    where Var = toVar
pattern Var' x <- Var _ x

class ConstC attr s c | s -> c, s -> attr where
    toConst :: attr -> c -> s
    fromConst :: s -> Maybe (attr, c)
pattern Const :: (ConstC attr s c) => attr -> c -> s
pattern Const attr c <- (fromConst -> Just (attr, c))
    where Const = toConst
pattern Const' c <- Const _ c

class AbsC attr s x | s -> x, s -> attr where
    toAbs :: attr -> x -> s -> s
    fromAbs :: s -> Maybe (attr, x, s)
pattern Abs :: (AbsC attr s x) => attr -> x -> s -> s
pattern Abs attr x e <- (fromAbs -> Just (attr, x, e))
    where Abs = toAbs
pattern Abs' x e <- Abs _ x e

class BigAbsC attr s a | s -> a, s -> attr where
    toBigAbs :: attr -> a -> s -> s
    fromBigAbs :: s -> Maybe (attr, a, s)
pattern BigAbs :: (BigAbsC attr s a) => attr -> a -> s -> s
pattern BigAbs attr a e <- (fromBigAbs -> Just (attr, a, e))
    where BigAbs = toBigAbs
pattern BigAbs' a e <- BigAbs _ a e

class AppC attr s | s -> attr where
    toApp :: attr -> s -> s -> s
    fromApp :: s -> Maybe (attr, s, s)
pattern App :: (AppC attr s) => attr -> s -> s -> s
pattern App attr e1 e2 <- (fromApp -> Just (attr, e1, e2))
    where App = toApp
pattern App' e1 e2 <- App _ e1 e2

class BigAppC attr s t | s -> t, s -> attr where
    toBigApp :: attr -> s -> t -> s
    fromBigApp :: s -> Maybe (attr, s, t)
pattern BigApp :: (BigAppC attr s t) => attr -> s -> t -> s
pattern BigApp attr e t <- (fromBigApp -> Just (attr, e, t))
    where BigApp = toBigApp
pattern BigApp' e t <- BigApp _ e t

class ForallC attr t a | t -> a, t -> attr where
    toForall :: attr -> a -> t -> t
    fromForall :: t -> Maybe (attr, a, t)
pattern Forall :: (ForallC attr t a) => attr -> a -> t -> t
pattern Forall attr a t <- (fromForall -> Just (attr, a, t))
    where Forall = toForall
pattern Forall' a t <- Forall _ a t

class ExistsC attr t a | t -> a, t -> attr where
    toExists :: attr -> a -> t -> t
    fromExists :: t -> Maybe (attr, a, t)
pattern Exists :: (ExistsC attr t a) => attr -> a -> t -> t
pattern Exists attr a t <- (fromExists -> Just (attr, a, t))
    where Exists = toExists
pattern Exists' a t <- Exists _ a t



class ValueC s where
    isValue :: s -> Bool
pattern Value :: (ValueC s) => s -> s
pattern Value v <- (\e -> if isValue e then Just e else Nothing -> Just v)

class MonotypeC s where
    isMonotype :: s -> Bool
pattern Mono :: (MonotypeC s) => s -> s
pattern Mono t <- (\s -> if isMonotype s then Just s else Nothing -> Just t)