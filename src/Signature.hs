{-#LANGUAGE FlexibleInstances, FlexibleContexts,
            GADTs #-}
module Signature
    ( TermLevel, TypeLevel, KindLevel
    , C(..)
    , Id(..)
    , StrId(..)
    ) where


import Data.Maybe
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

