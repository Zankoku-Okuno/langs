{-#LANGUAGE FlexibleContexts, FlexibleInstances,
            MultiParamTypeClasses, TypeFamilies #-}
module Ident (
      Scope, runScope
    , identFor, scopeError, withFresh
    , Bind(..)
    , module Gensym
    ) where

import Control.Monad.Reader
import Control.Monad.Writer

import Data.List
import AList
import qualified AList as A
import Gensym


data Scope id a = S { unScope :: ReaderT (AList (SourceId id) id)
                                 (GensymT
                                 (Writer [SourceId id]))
                                                                   a }

runScope :: (Eq (SourceId id)) => Scope id a -> Either ([SourceId id]) a
runScope (S action) = case runWriter . runGensymT . flip runReaderT A.empty $ action of
    (val, []) -> Right val
    (_, errs) -> Left $ nub errs

identFor :: (Eq (SourceId id)) => SourceId id -> Scope id (Maybe id)
identFor sid = S $ asks $ \ctx -> ctx `A.lookup` sid

scopeError :: (MkId id) => SourceId id -> Scope id ()
scopeError sid = S $ lift . lift $ tell [sid]

withFresh :: (MkId id) => SourceId id -> (id -> Scope id a) -> Scope id a
withFresh sid action = do
    id <- sym sid
    S . local (\ctx -> update ctx sid id) . unScope $ action id
        --Univ id <$> scopeCheck t


class Bind f where
    scopeCheck :: (MkId id) => f (SourceId id) -> Scope id (f id)

    fv :: (Eq id) => f id -> [id]
    
    type Subst f id
    subst :: (Eq id) => Subst f id -> f id -> f id





instance Functor (Scope id) where
    fmap f (S x) = S $ f <$> x
instance Applicative (Scope id) where
    pure = S . pure
    (S f) <*> (S x) = S $ f <*> x
instance Monad (Scope id) where
    return = pure
    (S x) >>= f = S $ x >>= unScope . f
instance MonadGensym (Scope id) where
    sym = S . lift . sym
    gensym = S . lift . gensym
