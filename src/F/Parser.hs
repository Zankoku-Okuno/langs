{-#LANGUAGE FlexibleContexts #-}
module F.Parser where

import Text.Luthor
import Text.Luthor.Syntax

import Const
import F.Syntax


expr :: Arr tc => Parsec String () (Term c tc Sid)
expr = do
    f <- inst
    args <- many $ ws *> inst
    pure $ foldl App f args
    where
    inst = do
        f <- atom
        ts <- many $ optional ws *> inBrackets ty
        pure $ foldl TyApp f ts
    atom = inParens expr <||> abs <||> tyAbs <||> (Var . TermId <$> var)
        where
        abs = do
            x <- string "λ" *> annotVar
            e <- string "." *> optional ws *> expr
            pure $ Abs x e
        tyAbs = do
            a <- string "Λ" *> (TypeId <$> var)
            e <- string "." *> optional ws *> expr
            pure $ TyAbs a e
    annotVar = do
        x <- TermId <$> var
        optional ws >> string ":" >> optional ws
        t <- ty
        pure (x, t)

    mkApp f (Left e) = App f e
    mkApp f (Right t) = TyApp f t

ty :: Arr tc => Parsec String () (Type tc Sid)
ty = do
    t <- atom
    ts <- many $ between2 (optional ws) (string "->") *> atom
    pure $ foldr1 ArrTy (t:ts)
    where
    atom = inParens ty <||> univ <||> (TVar . TypeId <$> var)
    univ = do
        a <- string "∀" *> (TypeId <$> var)
        t <- string "." *> optional ws *> ty
        pure $ Univ a t

var = charClass "a-zA-Z0-9_" `many1Not` charClass "0-9"



ws :: Stream s m Char => ParsecT s a m ()
ws = many1_ (void lws <||> newline)