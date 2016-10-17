{-#LANGUAGE FlexibleContexts #-}
module F.Parser where

import Parser
import F.Syntax


expr :: ( Stream s m Char
        , ArrC attr (Type attr c id)
        , CtorC attr (Type attr c id) (c TypeLevel) (Type attr c id)
        , IsString (id TermLevel), IsString (id TypeLevel)
        , IsString (c TypeLevel)
        , IsSourcePos attr
        ) => ParsecT s any m (Term attr c id)
expr = between2 ws0 $ call
    where
    call = app inst (ws *> inst) <||> inst
    inst = bigapp atom (ws0 *> inBrackets ty) <||> atom
    atom = choice [ inParens expr
                  , lambda (annotate lovar ty) expr
                  , bigLambda lovar expr
                  , var lovar
                  ]

ty :: ( Stream s m Char
      , ArrC attr (Type attr c id)
      , CtorC attr (Type attr c id) (c TypeLevel) (Type attr c id)
      , IsString (id TypeLevel), IsString (c TypeLevel)
      , IsSourcePos attr
      ) => ParsecT s any m (Type attr c id)
ty = between2 ws0 $ arrow call
    where
    call = ctor upvar atom <||> atom
    atom = choice [ inParens ty
                  , forall lovar ty
                  , var lovar
                  ]
