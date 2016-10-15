{-#LANGUAGE LambdaCase, FlexibleContexts, UndecidableInstances #-}
module F.Typecheck where

import Data.Maybe
import Control.Monad.Reader

import F.Syntax
import F.Unify
import F.Scope hiding (Context(..))

import Control.Monad.Writer


data Context attr c id = Ctx
    { terms :: [(id TermLevel, Type attr c id)]
    , consts :: c TermLevel -> Maybe (Type attr c id)
    , ops :: c TypeLevel -> Maybe Int
    }


data TcError attr c id
    = NoUnify (Term attr c id)       (Type attr c id) (Type attr c id)
    | NoInstance (Term attr c id)    (Type attr c id) (Type attr c id)
    | UnknownTypeOp (Type attr c id) (c TypeLevel)
    | TypeOpArity (Type attr c id)   (c TypeLevel, Int) [Type attr c id]
instance ( Show attr, Show (c TermLevel), Show (c TypeLevel)
         , Show (Type attr c id) -- FIXME to have this, I need undecidable instances
         ) => Show (TcError attr c id) where
    show (NoUnify e t1 t2) = concat ["Could not unify ", show t1, " with ", show t2]
    show (NoInstance e sigma t) = concat ["Could not instantiate ", show sigma, " at ", show t]
    show (UnknownTypeOp t c) = concat ["Unknown type operator at ???:\n\t", show t]
    show (TypeOpArity t (c, arity) ts) = concat ["Type operator ", show c, " expected ", show arity, " arguments , but got ", show $ length ts]
type Tc attr c id a = ReaderT (Context attr c id)
                      (Writer [TcError attr c id])
                      a


typeCheck :: ( Eq (id TermLevel), Eq (id TypeLevel)
             , Eq (c TermLevel), Eq (c TypeLevel)
             , ArrC (c TypeLevel)
             ) => Context attr c id
               -> Term attr c id
               -> Either [TcError attr c id] (Type attr c id)
typeCheck ctx0 e = case runWriter $ runReaderT (checkType e) ctx0 of
    (Just t, []) -> Right t
    (Nothing, errs) -> Left errs


checkType :: ( Eq (id TermLevel), Eq (id TypeLevel)
             , Eq (c TermLevel), Eq (c TypeLevel)
             , ArrC (c TypeLevel)
             ) => Term attr c id -> Tc attr c id (Maybe (Type attr c id))
checkType (Var' x) = do
    t <- asks (lookup x . terms)
    when (isNothing t) $ error "scope error: this should have been caught by earlier compilation"
    pure t
checkType (Const' c) = do
    t <- asks (($ c) . consts)
    when (isNothing t) $ error "unknown constant error: this should not have been introduced by earlier compilation"
    pure t
checkType (Abs' (x, t1) e) = do
    k <- checkKind t1
    ctx <- ask
    let ctx' = ctx { terms = (x, t1) : terms ctx }
    t <- local (const ctx') (checkType e)
    case (k, t) of
        (Just _, Just t2) -> pure . Just $ Arr undefined t1 t2
        _ -> pure Nothing
checkType e0@(App' e1 e2) = do
    tf <- checkType e1
    t1 <- checkType e2
    case (tf, t1) of
        (Just tf, Just t1) -> do
            let t2 = unifyFun tf t1
            when (isNothing t2) $ tell [NoUnify e0 tf t1]
            pure t2
        _ -> pure Nothing
checkType (BigAbs' a e) = do
    t <- checkType e
    pure $ Forall undefined a <$> t
checkType e0@(BigApp' e t) = do
    k <- checkKind t
    sigma <- checkType e
    case (k, sigma) of
        (Just _, Just sigma) -> do
            let t' = inst sigma t
            when (isNothing t') $ tell [NoInstance e0 sigma t]
            pure t'
        _ -> pure Nothing


checkKind :: (Eq (c TypeLevel)) => Type attr c id -> Tc attr c id (Maybe ())
checkKind (Var' x) = pure $ Just ()
checkKind t0@(Ctor' c ts) = do
    let numArgs = length ts
    asks (($ c) . ops) >>= \case
        Just arity
            | numArgs == arity -> pure $ Just ()
            | otherwise -> tell [TypeOpArity t0 (c, arity) ts] >> pure Nothing
        Nothing -> tell [UnknownTypeOp t0 c] >> pure Nothing
checkKind (Forall' a t) = checkKind t
