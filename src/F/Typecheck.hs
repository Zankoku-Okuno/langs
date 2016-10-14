{-#LANGUAGE FlexibleContexts #-}
module F.Typecheck where

import Control.Monad.Reader

import F.Syntax
import F.Unify
import F.Scope hiding (Context(..))

data Context attr c op id = Ctx
    { terms :: [(id TermLevel, Type attr op id)]
    , consts :: [(c, Type attr op id)]
    }


typeCheck :: (Eq (id TermLevel), Eq (id TypeLevel),
                Eq c, Eq op, ArrC op) =>
             Context attr c op id -> Term attr c op id -> Maybe (Type attr op id)
typeCheck ctx0 e = runReaderT (go e) ctx0
    where
    go (Var' x) = lift . lookup x =<< asks terms
    go (Const' c) = lift . lookup c =<< asks consts
    go (Abs' (x, t1) e) = do
        ctx <- ask
        let ctx' = ctx { terms = (x, t1) : terms ctx }
        t2 <- local (const ctx') (go e)
        pure $ Arr undefined t1 t2
    go (App' e1 e2) = do
        tf <- go e1
        t <- go e2
        lift $ unifyFun tf t
    go (BigAbs' a e) = Forall undefined a <$> go e
    go (BigApp' e t) = do
        sigma <- go e
        lift $ inst sigma t
