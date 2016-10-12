module F.Parser where

import Text.Luthor
import Text.Luthor.Syntax

import Const
import F.Syntax


expr :: Arr tc => Parsec String () (Term c tc Sid)
expr = do
    f <- atom
    args <- many $ string " " *> (Left <$> atom <||> Right <$> inBrackets ty)
    pure $ foldl mkApp f args
    where
    atom = inParens expr <||> abs <||> tyAbs <||> (Var . TermVar <$> var)
        where
        abs = do
            x <- string "λ" *> annotVar
            e <- string ". " *> expr
            pure $ Abs x e
        tyAbs = do
            a <- string "Λ" *> (TypeVar <$> var)
            e <- string ". " *> expr
            pure $ TyAbs a e
    annotVar = do
        x <- TermVar <$> var
        string ":"
        t <- ty
        pure (x, t)

    mkApp f (Left e) = App f e
    mkApp f (Right t) = TyApp f t

ty :: Arr tc => Parsec String () (Type tc Sid)
ty = do
    t <- atom
    ts <- many $ string " -> " *> atom
    pure $ foldr1 ArrTy (t:ts)
    where
    atom = inParens ty <||> univ <||> (TVar . TypeVar <$> var)
    univ = do
        a <- string "∀" *> (TypeVar <$> var)
        t <- string ". " *> ty
        pure $ Univ a t

var = charClass "a-zA-Z0-9_" `many1Not` charClass "0-9"