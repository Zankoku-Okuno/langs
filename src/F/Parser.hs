{-#LANGUAGE FlexibleContexts #-}
module F.Parser where

import Data.String (IsString(..))
import Text.Luthor
import Text.Luthor.Syntax

import F.Syntax

-- FIXME change attr from () to some kind of SourceLocation


expr :: ( ArrC () (Type () c id)
        , CtorC () (Type () c id) (c TypeLevel) (Type () c id)
        , IsString (id TermLevel), IsString (id TypeLevel)
        , IsString (c TypeLevel)
        ) => Parsec String () (Term () c id)
expr = between2 (optional ws) $ do
    f <- inst
    args <- many $ ws *> inst
    pure $ foldl (App ()) f args
    where
    inst = do
        f <- atom
        ts <- many $ optional ws *> inBrackets ty
        pure $ foldl (BigApp ()) f ts
    atom = inParens expr <||> abs <||> tyAbs <||> (Var () . fromString <$> var)
        where
        abs = do
            x <- string "λ" *> annotVar
            e <- string "." *> optional ws *> expr
            pure $ Abs () x e
        tyAbs = do
            a <- string "Λ" *> (fromString <$> var)
            e <- string "." *> optional ws *> expr
            pure $ BigAbs () a e
    annotVar = do
        x <- fromString <$> var
        optional ws >> string ":" >> optional ws
        t <- ty
        pure (x, t)


ty :: ( ArrC () (Type () c id)
      , CtorC () (Type () c id) (c TypeLevel) (Type () c id)
      , IsString (id TypeLevel), IsString (c TypeLevel)
      ) => Parsec String () (Type () c id)
ty = between2 (optional ws) $ do
    t <- app
    ts <- many $ between2 (optional ws) (string "->") *> app
    pure $ foldr1 (Arr ()) (t:ts)
    where
    app = _app <||> atom
        where
        _app = do
            c <- fromString <$> upVar
            ts <- many $ ws *> atom
            pure $ Ctor () c ts
    atom = inParens ty <||> univ <||> (Var () . fromString <$> var)
    univ = do
        a <- string "∀" *> (fromString <$> var)
        t <- string "." *> optional ws *> ty
        pure $ Forall () a t

var = charClass "a-zA-Z0-9_" `many1Not` charClass "0-9"
upVar = charClass "a-zA-Z0-9_" `many1Not` charClass "a-z0-9"



ws :: Stream s m Char => ParsecT s a m ()
ws = many1_ (void lws <||> newline)