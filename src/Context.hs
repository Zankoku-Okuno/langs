{-#LANGUAGE PatternSynonyms, PolyKinds #-}
module Context
    ( Gamma, IsGamma(..)
    , Context(..)
    -- FIXME these should be in a class
    , Constants(..)
    , termConsts, typeConsts
    , Paramd(..), Nada, pattern Nada
    ) where

import Data.Maybe (isJust)
import Data.Monoid
import Control.Applicative

import Identifier


data Context attr c id terms types kinds = Ctx
    { gamma :: Gamma attr c id terms types kinds
    , constants :: Constants attr c id terms types kinds
    }
instance Monoid (Context attr c id terms types kinds) where
    mempty = Ctx mempty mempty
    (Ctx g1 c1) `mappend` (Ctx g2 c2) = Ctx (g1 <> g2) (c1 <> c2)

-- The datatype `Paramd` is there pure to wrap up types `a :: *` into
-- type constructors suitable for contexts
-- (which must have kind `* -> (* -> *) -> (* -> *) -> *)
data Paramd a attr (c :: * -> *) (id :: * -> *) = Paramd a
type Nada = Paramd ()
pattern Nada = Paramd ()


data Gamma attr (c :: * -> *) (id :: * -> *) terms types kinds = Gamma
    { termGamma :: [(id TermLevel, terms attr c id)]
    , typeGamma :: [(id TypeLevel, types attr c id)]
    , kindGamma :: [(id KindLevel, kinds attr c id)]
    }

instance Monoid (Gamma attr c id terms types kinds) where
    mempty = Gamma
        { termGamma = []
        , typeGamma = []
        , kindGamma = []
        }
    a `mappend` b = Gamma
        { termGamma = termGamma a <> termGamma b
        , typeGamma = typeGamma a <> typeGamma b
        , kindGamma = kindGamma a <> kindGamma b
        }


data Constants attr (c :: * -> *) (id :: * -> *) terms types kinds = Constants
    { termConstants :: c TermLevel -> Maybe (terms attr c id)
    , typeConstants :: c TypeLevel -> Maybe (types attr c id)
    , kindConstants :: c KindLevel -> Maybe (kinds attr c id)
    }

instance Monoid (Constants attr c id terms types kinds) where
    mempty = Constants
        { termConstants = const Nothing
        , typeConstants = const Nothing
        , kindConstants = const Nothing
        }
    a `mappend` b = Constants
        { termConstants = \c -> termConstants a c <|> termConstants b c
        , typeConstants = \c -> typeConstants a c <|> typeConstants b c
        , kindConstants = \c -> kindConstants a c <|> kindConstants b c
        }


class IsGamma g where
    terms :: g attr c id terms types kinds -> [(id TermLevel, terms attr c id)]
    types :: g attr c id terms types kinds -> [(id TypeLevel, types attr c id)]
    kinds :: g attr c id terms types kinds -> [(id KindLevel, kinds attr c id)]
    
    getTerm :: (Eq (id TermLevel)) => g attr c id terms types kinds -> id TermLevel -> Maybe (terms attr c id)
    getTerm theta id = lookup id $ terms theta
    getType :: (Eq (id TypeLevel)) => g attr c id terms types kinds -> id TypeLevel -> Maybe (types attr c id)
    getType theta id = lookup id $ types theta
    getKind :: (Eq (id KindLevel)) => g attr c id terms types kinds -> id KindLevel -> Maybe (kinds attr c id)
    getKind theta id = lookup id $ kinds theta

    hasTerm :: (Eq (id TermLevel)) => g attr c id terms types kinds -> id TermLevel -> Bool
    hasTerm theta id = isJust $ getTerm theta id
    hasType :: (Eq (id TypeLevel)) => g attr c id terms types kinds -> id TypeLevel -> Bool
    hasType theta id = isJust $ getType theta id
    hasKind :: (Eq (id KindLevel)) => g attr c id terms types kinds -> id KindLevel -> Bool
    hasKind theta id = isJust $ getKind theta id

    addTerm :: (id TermLevel, terms attr c id) -> g attr c id terms types kinds -> g attr c id terms types kinds
    addType :: (id TypeLevel, types attr c id) -> g attr c id terms types kinds -> g attr c id terms types kinds
    addKind :: (id KindLevel, kinds attr c id) -> g attr c id terms types kinds -> g attr c id terms types kinds
    
    delTerm :: (Eq (id TermLevel)) => id TermLevel -> g attr c id terms types kinds -> g attr c id terms types kinds
    delType :: (Eq (id TypeLevel)) => id TypeLevel -> g attr c id terms types kinds -> g attr c id terms types kinds
    delKind :: (Eq (id KindLevel)) => id KindLevel -> g attr c id terms types kinds -> g attr c id terms types kinds

instance IsGamma Gamma where
    terms = termGamma
    types = typeGamma
    kinds = kindGamma

    addTerm add theta0 = theta0 { termGamma = add : termGamma theta0 }
    addType add theta0 = theta0 { typeGamma = add : typeGamma theta0 }
    addKind add theta0 = theta0 { kindGamma = add : kindGamma theta0 }

    delTerm del theta0 = theta0 { termGamma = [ it | it <- termGamma theta0, fst it /= del] }
    delType del theta0 = theta0 { typeGamma = [ it | it <- typeGamma theta0, fst it /= del] }
    delKind del theta0 = theta0 { kindGamma = [ it | it <- kindGamma theta0, fst it /= del] }

instance IsGamma Context where
    terms = termGamma . gamma
    types = typeGamma . gamma
    kinds = kindGamma . gamma

    addTerm add ctx = ctx { gamma = addTerm add $ gamma ctx }
    addType add ctx = ctx { gamma = addType add $ gamma ctx }
    addKind add ctx = ctx { gamma = addKind add $ gamma ctx }

    delTerm del theta0 = theta0 { gamma = delTerm del (gamma theta0) }
    delType del theta0 = theta0 { gamma = delType del (gamma theta0) }
    delKind del theta0 = theta0 { gamma = delKind del (gamma theta0) }


--FIXME make these part of a class
termConsts = termConstants . constants
typeConsts = typeConstants . constants
kindConsts = kindConstants . constants


