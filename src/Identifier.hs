{-#LANGUAGE PatternSynonyms, ViewPatterns,
            FlexibleInstances, FlexibleContexts,
            GADTs, PolyKinds,
            MultiParamTypeClasses, FunctionalDependencies #-}
module Identifier
    ( TermLevel, TypeLevel, KindLevel
    , C(..)
    , Id(..)
    , StrId(..)
    , Gamma, IsGamma(..)
    , Constants(..)
    , Context(..)
    , Substitute(..), Paramd(..), Nada(..)
    , substTerm, substType, substKind
    -- FIXME these should be in a class
    , termConsts, typeConsts
    ) where


import Data.String (IsString(..))
import Data.Monoid
import Control.Applicative

------ Phantom Types ------

data TermLevel
data TypeLevel
data KindLevel


------ Constants ------

data C a level where
    TermC :: a -> C a TermLevel
    TypeC :: a -> C a TypeLevel
    KindC :: a -> C a KindLevel

instance Eq a => Eq (C a level) where
    (TermC x) == (TermC y) = x == y
    (TypeC a) == (TypeC b) = a == b
    (KindC k) == (KindC k') = k == k'

instance Show a => Show (C a level) where
    show (TermC x) = show x
    show (TypeC a) = show a
    show (KindC k) = show k


------ Identifiers ------


{-  Identifiers can appear at any of the semantic levels (term, type, kind), and so need to be parameterized by level.
    The `Id a` datatype takes an existing datum of type `a` and gives it the phantom type for its level.
    Importantly, the `a` type may need to store differrent information based on where it is in the semantic hierarchy.
    Again, we do this through the phantom type for that level: `a :: Level -> *`.
    Unfortunately, this means we can't just build `Id`s out of `String`s, but we can use `StrId` below,
    which just stores strings but accommodates the phantom type.

    That is: in naive parsing, it is probably enough to parse an identifier at the type `Id StrId level`.
    Since that type belongs to `IsString`, you can also type your identifier parser like
        `IsString id => Parser id`
    and if you specify higher in the call stack that you expect some `Id StrId level`, then everything
    should get wrapped up neatly by a simple `fromString` application in your parser's definition.
    NOTE: There's a default implementation of `Show (StrId level)` which prepends a tick for type ids and two for kind ids
    but to the string identifier itself (not the shown string, so no outer quotes, or escaping).
    If you need to show these identifiers differently, make your own type and give it an IsString implentation.
-}

data Id a level where
    TermId :: a TermLevel -> Id a TermLevel
    TypeId :: a TypeLevel -> Id a TypeLevel
    KindId :: a KindLevel -> Id a KindLevel

instance (Eq (a level)) => Eq (Id a level) where
    (TermId x) == (TermId y) = x == y
    (TypeId a) == (TypeId b) = a == b
    (KindId k) == (KindId k') = k == k'

instance (Show (a level)) => Show (Id a level) where
    show (TermId x) = show x
    show (TypeId a) = show a
    show (KindId k) = show k



data StrId level where
    TermStrId :: String -> StrId TermLevel
    TypeStrId :: String -> StrId TypeLevel
    KindStrId :: String -> StrId KindLevel
instance Eq (StrId level) where
    (TermStrId x) == (TermStrId y) = x == y
    (TypeStrId a) == (TypeStrId b) = a == b
    (KindStrId k1) == (KindStrId k2) = k1 == k2
instance Show (StrId level) where
    show (TermStrId x) = x
    show (TypeStrId a) = "'"++a
    show (KindStrId k) = "''"++k
instance IsString (Id StrId TermLevel) where
    fromString = TermId . TermStrId
instance IsString (Id StrId TypeLevel) where
    fromString = TypeId . TypeStrId
instance IsString (Id StrId KindLevel) where
    fromString = KindId . KindStrId


------ Contexts ------

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


data Context attr c id terms types kinds = Ctx
    { gamma :: Gamma attr c id terms types kinds
    , constants :: Constants attr c id terms types kinds
    }
instance Monoid (Context attr c id terms types kinds) where
    mempty = Ctx mempty mempty
    (Ctx g1 c1) `mappend` (Ctx g2 c2) = Ctx (g1 <> g2) (c1 <> c2)


class IsGamma g where
    terms :: g attr c id terms types kinds -> [(id TermLevel, terms attr c id)]
    types :: g attr c id terms types kinds -> [(id TypeLevel, types attr c id)]
    kinds :: g attr c id terms types kinds -> [(id KindLevel, kinds attr c id)]
    
    addTerm :: (id TermLevel, terms attr c id) -> g attr c id terms types kinds -> g attr c id terms types kinds
    addType :: (id TypeLevel, types attr c id) -> g attr c id terms types kinds -> g attr c id terms types kinds
    addKind :: (id KindLevel, kinds attr c id) -> g attr c id terms types kinds -> g attr c id terms types kinds
    -- FIXME can I eliminate the Eq constraint here?
    delTerm :: (Eq (id TermLevel)) => id TermLevel -> g attr c id terms types kinds -> g attr c id terms types kinds
    delType :: (Eq (id TypeLevel)) => id TypeLevel -> g attr c id terms types kinds -> g attr c id terms types kinds
    delKind :: (Eq (id KindLevel)) => id KindLevel -> g attr c id terms types kinds -> g attr c id terms types kinds
    
    -- TODO hasTerm, getTerm

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


termConsts = termConstants . constants
typeConsts = typeConstants . constants
kindConsts = kindConstants . constants


------ Substitution ------

class Substitute theta expr | expr -> theta where
    subst :: theta -> expr -> expr

substTerm :: (Substitute (Gamma attr c id terms types kinds) syntax) => (id TermLevel, terms attr c id) -> syntax -> syntax
substTerm theta e = addTerm theta mempty `subst` e
substType :: (Substitute (Gamma attr c id terms types kinds) syntax) => (id TypeLevel, types attr c id) -> syntax -> syntax
substType theta t = addType theta mempty `subst` t
substKind :: (Substitute (Gamma attr c id terms types kinds) syntax) => (id KindLevel, kinds attr c id) -> syntax -> syntax
substKind theta k = addKind theta mempty `subst` k



-- The datatype `Paramd` is there pure to wrap up types `a :: *` into
-- type constructors suitable for contexts
-- (which must have kind `* -> (* -> *) -> (* -> *) -> *)
data Paramd a attr (c :: * -> *) (id :: * -> *) = Paramd a
type Nada = Paramd ()
pattern Nada = Paramd ()
