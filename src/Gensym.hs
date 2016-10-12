{-#LANGUAGE FlexibleContexts, FlexibleInstances,
            MultiParamTypeClasses, TypeFamilies #-}
module Gensym
    ( MkId(..)
    , GensymT, Gensym, runGensymT, runGensym
    , MonadGensym(..)
    , Unique
    ) where

import Data.Unique
import System.IO.Unsafe

import Data.Functor.Identity
import Control.Monad.Trans


class (Eq id, Eq (SourceId id)) => MkId id where
    type Args id :: *
    mkId :: Args id -> Unique -> id

    type SourceId id :: *
    fromSource :: SourceId id -> Unique -> id


newtype GensymT m a = GensymT { unGensymT :: m a }
type Gensym a = GensymT Identity a

runGensymT :: GensymT m a -> m a
runGensymT = unGensymT

runGensym :: Gensym a -> a
runGensym = runIdentity . unGensymT

class Monad m => MonadGensym m where
    sym :: (MkId id) => SourceId id -> m id
    gensym :: (MkId id) => Args id -> m id

instance (Monad m) => MonadGensym (GensymT m) where
    sym source = pure . unsafePerformIO $ do
        uid <- newUnique
        return $ fromSource source uid

    gensym args = pure . unsafePerformIO $ do
        uid <- newUnique
        return $ mkId args uid



instance Functor m => Functor (GensymT m) where
    fmap f (GensymT x) = GensymT $ f <$> x
instance Applicative m => Applicative (GensymT m) where
    pure = GensymT . pure
    (GensymT f) <*> (GensymT x) = GensymT $ f <*> x
instance Monad m => Monad (GensymT m) where
    return = pure
    (GensymT x) >>= f = GensymT $ x >>= unGensymT . f
instance MonadTrans GensymT where
    lift = GensymT
