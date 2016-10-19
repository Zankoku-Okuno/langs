{-#LANGUAGE LambdaCase, FlexibleContexts, UndecidableInstances #-}
module F.Typecheck (typeCheck) where

import Data.Default
import Data.Maybe
import Control.Monad.Reader

import Context
import F.Syntax
import F.Scope
import F.Unify

import Control.Monad.Writer


data TcError attr c id
    = TermScope (Term attr c id) (id TermLevel)
    | TypeScope (Type attr c id) (id TypeLevel)
    
    | NoTermConstant (Term attr c id) (c TermLevel)
    | NoTypeConstant (Type attr c id) (c TypeLevel)
    | NoKindConstant (Kind attr c id) (c KindLevel)
    
    | NoUnifyFun     (Term attr c id) (Type attr c id) (Type attr c id)
    | NoUnifyTypeFun (Type attr c id) (Kind attr c id) (Kind attr c id)
    | NoInstance     (Term attr c id) (Type attr c id) (Type attr c id)
    | KindOpArity    (Kind attr c id) (c KindLevel, Int) [Kind attr c id]

instance ( Show attr
         , Show (id TermLevel), Show (id TypeLevel)
         , Show (c TermLevel), Show (c TypeLevel), Show (c KindLevel)
         , Show (Type attr c id), Show (Kind attr c id) -- FIXME to have this, I need undecidable instances
         ) => Show (TcError attr c id) where
    show (TermScope e x) = concat ["Variable not in scope: ", show x]
    show (TypeScope t a) = concat ["Type variable not in scope: ", show a]
    show (NoTermConstant e c) = concat ["No such term constant: ", show c]
    show (NoTypeConstant e c) = concat ["No such type constant: ", show c]
    show (NoKindConstant e c) = concat ["No such kind constant: ", show c]
    show (NoUnifyFun e t1 t2) = concat ["Could not call function of type ", show t1, " with argument type ", show t2]
    show (NoUnifyTypeFun t k1 k2) = concat ["Could not apply type operator of kind ", show k1, " with argument kind ", show k2]
    show (NoInstance e sigma t) = concat ["Could not instantiate ", show sigma, " at ", show t]
    show (KindOpArity t (c, arity) ts) = concat ["Kind operator ", show c, " expected ", show arity, " arguments , but got ", show $ length ts]

type Tc attr c id a = ReaderT (Context attr c id Type Kind (Paramd Int))
                      (Writer [TcError attr c id])
                      a


typeCheck :: ( Eq (id TermLevel), Eq (id TypeLevel)
             , Eq (c TermLevel), Eq (c TypeLevel), Eq (c KindLevel)
             , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
             , Default attr
             ) => Context attr c id Type Kind (Paramd Int)
               -> Term attr c id
               -> Either [TcError attr c id] (Type attr c id)
typeCheck ctx0 e = case runWriter $ runReaderT (checkType e) ctx0 of
    (Just t, []) -> Right t
    (Nothing, errs) -> Left errs


checkType :: ( Eq (id TermLevel), Eq (id TypeLevel)
             , Eq (c TermLevel), Eq (c TypeLevel), Eq (c KindLevel)
             , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
             , Default attr
             ) => Term attr c id -> Tc attr c id (Maybe (Type attr c id))
checkType e0@(Var' x) = do
    t <- asks (`getTerm` x)
    when (isNothing t) $ tell [TermScope e0 x]
    pure t
checkType e0@(Const' c) = do
    t <- asks (($ c) . termConsts)
    when (isNothing t) $ tell [NoTermConstant e0 c]
    pure t
checkType (Abs' (x, t1) e) = do
    k <- checkKind t1
    t <- local (addTerm (x, t1)) (checkType e)
    case (k, t) of
        (Just _, Just t2) -> pure . Just $ Arr def t1 t2
        _ -> pure Nothing
checkType e0@(App' e1 e2) = do
    tf <- checkType e1
    t1 <- checkType e2
    case (tf, t1) of
        (Just tf, Just t1) -> do
            let t2 = unifyFun tf t1
            when (isNothing t2) $ tell [NoUnifyFun e0 tf t1]
            pure t2
        _ -> pure Nothing
checkType (BigAbs' (a, k) e) = do
    checkArity k >>= \case
        Nothing -> pure Nothing
        Just _ -> do
            t <- local (addType (a, k)) $ checkType e
            pure $ Forall def (a, k) <$> t
checkType e0@(BigApp' e t) = do
    k <- checkKind t
    sigma <- checkType e
    case (k, sigma) of
        (Just k, Just sigma) -> do
            let t' = inst sigma (t, k)
            when (isNothing t') $ tell [NoInstance e0 sigma t]
            pure t'
        _ -> pure Nothing


checkKind :: ( Eq (id TypeLevel), Eq (c TypeLevel), Eq (c KindLevel)
             , ArrC attr (Kind attr c id)
             ) => Type attr c id -> Tc attr c id (Maybe (Kind attr c id))
checkKind t0@(Var' a) = do
    k <- asks (`getType` a)
    when (isNothing k) $ tell [TypeScope t0 a]
    pure k
checkKind t0@(Const' c) = do
    k <- asks (($ c) . typeConsts)
    when (isNothing k) $ tell [NoTypeConstant t0 c]
    pure k
checkKind t0@(App' t1 t2) = do
    kf <- checkKind t1
    k1 <- checkKind t2
    case (kf, k1) of
        (Just kf, Just k1) -> do
            let k2 = unifyTypeFun kf k1
            when (isNothing k2) $ tell [NoUnifyTypeFun t0 kf k1]
            pure k2
        _ -> pure Nothing
checkKind (Forall' (a, k) t) = do
    checkArity k >>= \case
        Nothing -> pure Nothing
        Just _ -> local (addType (a, k)) $ checkKind t


checkArity :: (Eq (c KindLevel)) => Kind attr c id -> Tc attr c id (Maybe ())
checkArity k0@(Ctor' c ks) = do
    let numArgs = length ks
    asks (($ c) . kindConsts) >>= \case
        Just (Paramd arity)
            | numArgs == arity -> pure $ Just ()
            | otherwise -> tell [KindOpArity k0 (c, arity) ks] >> pure Nothing
        Nothing -> tell [NoKindConstant k0 c] >> pure Nothing