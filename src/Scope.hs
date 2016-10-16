{-#LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module Scope where

import Identifier
import Context


class Substitute theta expr | expr -> theta where
    subst :: theta -> expr -> expr

substTerm :: (Substitute (Gamma attr c id terms types kinds) syntax) => (id TermLevel, terms attr c id) -> syntax -> syntax
substTerm theta e = addTerm theta mempty `subst` e
substType :: (Substitute (Gamma attr c id terms types kinds) syntax) => (id TypeLevel, types attr c id) -> syntax -> syntax
substType theta t = addType theta mempty `subst` t
substKind :: (Substitute (Gamma attr c id terms types kinds) syntax) => (id KindLevel, kinds attr c id) -> syntax -> syntax
substKind theta k = addKind theta mempty `subst` k



-- FIXME stuff to get free variables