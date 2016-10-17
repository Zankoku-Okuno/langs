{-#LANGUAGE FlexibleContexts #-}
module FOmegaNoLambda.Parser where

import Parser
import FOmegaNoLambda.Syntax


expr :: ( Stream s m Char
        , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
        , CtorC attr (Kind attr c id) (c KindLevel) (Kind attr c id)
        , IsString (id TermLevel), IsString (id TypeLevel)
        , IsString (c TypeLevel), IsString (c KindLevel)
        , IsSourcePos attr
        ) => ParsecT s any m (Term attr c id)
expr = between2 ws0 $ call
    where
    call = app inst (ws *> inst) <||> inst
    inst = bigapp atom (ws0 *> inBrackets ty) <||> atom
    atom = choice [ inParens expr
                  , lambda (annotate lovar ty) expr
                  , bigLambda (annotate lovar kind) expr
                  , var lovar
                  ]

ty :: ( Stream s m Char
      , ArrC attr (Type attr c id), ArrC attr (Kind attr c id)
      , CtorC attr (Kind attr c id) (c KindLevel) (Kind attr c id)
      , IsString (id TypeLevel), IsString (c TypeLevel), IsString (c KindLevel)
      , IsSourcePos attr
      ) => ParsecT s any m (Type attr c id)
ty = between2 ws0 $ arrow call
    where
    call = app atom (ws *> atom) <||> atom
    atom = choice [ inParens ty
                  , forall (annotate lovar kind) ty
                  , var lovar
                  , const
                  ]
    const = do
        (l1, c, l2) <- withPositions $ fromString <$> upvar
        pure $ Const (fromSourcePos l1 l2) c

kind :: ( Stream s m Char
        , ArrC attr (Kind attr c id)
        , CtorC attr (Kind attr c id) (c KindLevel) (Kind attr c id)
        , IsString (c KindLevel)
        , IsSourcePos attr
        ) => ParsecT s any m (Kind attr c id)
kind = between2 ws0 $ arrow atom
    where
    atom = inParens kind <||> unit
    unit = do
        (l1, it, l2) <- withPositions $ fromString <$> string "*"
        pure $ Ctor (fromSourcePos l1 l2) it []
