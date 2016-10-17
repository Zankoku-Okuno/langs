{-#LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Parser
    ( ws, ws0
    , lovar, upvar
    , annotate
    , var
    , binder, lambda, bigLambda, forall
    , app, bigapp, ctor, arrow
    , IsSourcePos(..)
    -- re-exports
    , IsString(..)
    , module Luthor
    ) where

import Data.String (IsString(..))
import Text.Luthor as Luthor
import Text.Luthor.Syntax as Luthor

import Syntax


class IsSourcePos a where
    fromSourcePos :: SourcePos -> SourcePos -> a
instance IsSourcePos () where
    fromSourcePos _ _ = ()
instance IsSourcePos SourcePos where
    fromSourcePos l _ = l
instance IsSourcePos (SourcePos, SourcePos) where
    fromSourcePos l1 l2 = (l1, l2)


ws, ws0 :: Stream s m Char => ParsecT s a m ()
ws = many1_ (void lws <||> newline)
ws0 = optional_ ws

lovar, upvar :: (Stream s m Char, IsString str) => ParsecT s a m str
lovar = _var "A-Z"
upvar = _var "a-z"
_var no = do
    underscores <- many $ char '_'
    rest <- charClass "a-zA-Z0-9_" `many1Not` charClass (no ++ "0-9")
    pure . fromString $ underscores ++ rest

annotate :: Stream s m Char
            => ParsecT s a m body
            -> ParsecT s a m annot
            -> ParsecT s a m (body, annot)
annotate body annot = do
    e <- body
    between2 ws0 (char ':')
    t <- annot
    pure (e, t)


var :: (Stream s m Char, IsSourcePos attr
       , VarC attr x id
       ) => ParsecT s a m id -> ParsecT s a m x
var id = do
    (l1, id, l2) <- withPositions id
    pure $ Var (fromSourcePos l1 l2) id

binder :: (Stream s m Char)
          => String
          -> ParsecT s a m bind
          -> ParsecT s a m body
          -> ParsecT s a m (bind, body)
binder lead bind body = do
    string lead <* ws0
    -- TODO allow multiple binders
    x <- inParens bind <||> bind
    between2 ws0 (char '.')
    e <- body
    return (x, e)

lambda :: ( Stream s m Char, IsSourcePos attr
          , AbsC attr body bind
          ) => ParsecT s a m bind
          -> ParsecT s a m body
          -> ParsecT s a m body
lambda bind body = do
    (l1, (bind, body), l2) <- withPositions $ binder "λ" bind body
    return $ Abs (fromSourcePos l1 l2) bind body

bigLambda :: ( Stream s m Char, IsSourcePos attr
             , BigAbsC attr body bind
             ) => ParsecT s a m bind
             -> ParsecT s a m body
             -> ParsecT s a m body
bigLambda bind body = do
    (l0, (bind, body), l1) <- withPositions $ binder "Λ" bind body
    return $ BigAbs (fromSourcePos l0 l1) bind body

forall :: ( Stream s m Char, IsSourcePos attr
          , ForallC attr body bind
          ) => ParsecT s a m bind
          -> ParsecT s a m body
          -> ParsecT s a m body
forall bind body = do
    (l1, (bind, body), l2) <- withPositions $ binder "∀" bind body
    return $ Forall (fromSourcePos l1 l2) bind body


ctor :: ( Stream s m Char, IsSourcePos attr
        , CtorC attr r ctor arg
        ) => ParsecT s a m ctor
        -> ParsecT s a m arg
        -> ParsecT s a m r
ctor ctor arg = do
    (c, l1) <- withPosition ctor
    (args, l2) <- withPositionEnd $ many $ ws *> arg
    pure $ Ctor (fromSourcePos l1 l2) c args

app :: ( Stream s m Char, IsSourcePos attr
       , AppC attr e
       ) => ParsecT s a m e
       -> ParsecT s a m e
       -> ParsecT s a m e
app f arg = do
    f <- withPosition f
    args <-  many1 $ withPositionEnd arg
    pure $ appMany f args
    where
    appMany (f, _) [] = f
    appMany (f, l1) args = 
        let (a, l2) = head args
            f' = App (fromSourcePos l1 l2) f a
        in appMany (f', l1) (tail args)


bigapp :: ( Stream s m Char, IsSourcePos attr
          , BigAppC attr f arg
          ) => ParsecT s a m f
          -> ParsecT s a m arg
          -> ParsecT s a m f
bigapp f arg = do
    f <- withPosition f
    args <-  many1 $ withPositionEnd arg
    pure $ appMany f args
    where
    appMany (f, _) [] = f
    appMany (f, l1) args = 
        let (a, l2) = head args
            f' = BigApp (fromSourcePos l1 l2) f a
        in appMany (f', l1) (tail args)

arrow :: ( Stream s m Char, IsSourcePos attr
         , ArrC attr body
         ) => ParsecT s a m body
         -> ParsecT s a m body
arrow body = do
    x <- withPositions body
    xs <- many $ withPositions $ between2 ws0 (string "->") *> body
    pure $ arrayMany (x:xs)
    --x <- body
    --xs <- many $ between2 ws0 (string "->") *> body
    --pure $ foldr1 (Arr ()) (x:xs)
    where
    arrayMany es = let (_, e, _) = go es in e
        where
        go [e] = e
        go es =
            let (l1, e1, _) = head es
                (_, e2, l2) = go (tail es)
            in (l1, Arr (fromSourcePos l1 l2) e1 e2, l2)