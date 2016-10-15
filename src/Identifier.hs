{-#LANGUAGE PatternSynonyms, ViewPatterns,
            FlexibleInstances,
            GADTs, KindSignatures,
            MultiParamTypeClasses, FunctionalDependencies #-}
module Identifier where


import Data.String (IsString(..))

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
emptyGamma :: Gamma attr c id terms types kinds
emptyGamma = Gamma
    { termGamma = []
    , typeGamma = []
    , kindGamma = []
    }

-- FIXME make a contexty class with addX, delX, hasX, getX
addTermGamma :: (id TermLevel, terms attr c id) -> Gamma attr c id terms types kinds -> Gamma attr c id terms types kinds
addTermGamma add theta0 = theta0 { termGamma = add : termGamma theta0 }
addTypeGamma :: (id TypeLevel, types attr c id) -> Gamma attr c id terms types kinds -> Gamma attr c id terms types kinds
addTypeGamma add theta0 = theta0 { typeGamma = add : typeGamma theta0 }
addKindGamma :: (id KindLevel, kinds attr c id) -> Gamma attr c id terms types kinds -> Gamma attr c id terms types kinds
addKindGamma add theta0 = theta0 { kindGamma = add : kindGamma theta0 }

delTermGamma :: Eq (id TermLevel) => id TermLevel -> Gamma attr c id terms types kinds -> Gamma attr c id terms types kinds
delTermGamma del theta0 = theta0 { termGamma = [ it | it <- termGamma theta0, fst it /= del] }
delTypeGamma :: Eq (id TypeLevel) => id TypeLevel -> Gamma attr c id terms types kinds -> Gamma attr c id terms types kinds
delTypeGamma del theta0 = theta0 { typeGamma = [ it | it <- typeGamma theta0, fst it /= del] }
delKindGamma :: Eq (id KindLevel) => id KindLevel -> Gamma attr c id terms types kinds -> Gamma attr c id terms types kinds
delKindGamma del theta0 = theta0 { kindGamma = [ it | it <- kindGamma theta0, fst it /= del] }


data Constants attr (c :: * -> *) (id :: * -> *) terms types kinds = Constants
    { termConstants :: c TermLevel -> Maybe (terms attr c id)
    , typeConstants :: c TypeLevel -> Maybe (types attr c id)
    , kindConstants :: c KindLevel -> Maybe (kinds attr c id)
    }
emptyConstants :: Constants attr c id terms types kinds
emptyConstants = Constants
    { termConstants = const Nothing
    , typeConstants = const Nothing
    , kindConstants = const Nothing
    }


data Context attr c id terms types kinds = Ctx
    { gamma :: Gamma attr c id terms types kinds
    , constants :: Constants attr c id terms types kinds
    }
emptyContext :: Context attr c id terms types kinds
emptyContext = Ctx
    { gamma = emptyGamma
    , constants = emptyConstants
    }

terms = termGamma . gamma
types = typeGamma . gamma
kinds = kindGamma . gamma

termConsts = termConstants . constants
typeConsts = typeConstants . constants
kindConsts = kindConstants . constants

addTerm :: (id TermLevel, terms attr c id) -> Context attr c id terms types kinds -> Context attr c id terms types kinds
addTerm add ctx = ctx { gamma = addTermGamma add $ gamma ctx }
addType :: (id TypeLevel, types attr c id) -> Context attr c id terms types kinds -> Context attr c id terms types kinds
addType add ctx = ctx { gamma = addTypeGamma add $ gamma ctx }
addKind :: (id KindLevel, kinds attr c id) -> Context attr c id terms types kinds -> Context attr c id terms types kinds
addKind add ctx = ctx { gamma = addKindGamma add $ gamma ctx }

-- The datatype `Paramd` is there pure to wrap up types `a :: *` into
-- type constructors suitable for contexts
-- (which must have kind `* -> (* -> *) -> (* -> *) -> *)
data Paramd a attr (c :: * -> *) (id :: * -> *) = Paramd a
type Nada = Paramd ()
pattern Nada = Paramd ()
