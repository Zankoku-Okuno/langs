{-#LANGUAGE LambdaCase #-}
module Lambda.Interpreter
    ( eval
    ) where

import Lambda.Syntax
import Lambda.Scope


type Delta m attr c id =  attr
                       -> c TermLevel
                       -> Term attr c id
                       -> m (Maybe (Term attr c id))

eval :: (Eq (id TermLevel), Monad m) =>
           Delta m attr c id
        -> Term attr c id
        -> m (Either (Term attr c id) (Term attr c id))
eval delta e = do
    nf <- go e
    pure $ case nf of
        Value v -> Right v
        e -> Left e
    where
    go (Value v) = pure v
    go (App attr e1 e2) = do
        e1 <- go e1
        e2 <- go e2
        let stuck = pure $ App attr e1 e2
        case (e1, e2) of
            (Abs' x e, v) -> go $ (x, v) `substTerm` e
            (Const attr c, v) -> delta attr c v >>= maybe stuck go
            _ -> stuck
