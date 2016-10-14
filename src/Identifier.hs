{-#LANGUAGE PatternSynonyms, ViewPatterns,
            FlexibleInstances,
            GADTs, KindSignatures,
            MultiParamTypeClasses, FunctionalDependencies #-}
module Identifier
    ( TermLevel, TypeLevel, KindLevel
    , IdC
    , pattern TermId, pattern TypeId, pattern KindId
    , SourceId(..)
    , DeBruijnId(..)
    ) where


data TermLevel
data TypeLevel
data KindLevel


class IdC id args | id -> args where
    toTermId :: args -> id TermLevel
    fromTermId :: id TermLevel -> args
    toTypeId :: args -> id TypeLevel
    fromTypeId :: id TypeLevel -> args
    toKindId :: args -> id KindLevel
    fromKindId :: id KindLevel -> args

pattern TermId :: (IdC id args) => args -> id TermLevel
pattern TermId args <- (fromTermId -> args)
    where TermId = toTermId
pattern TypeId :: (IdC id args) => args -> id TypeLevel
pattern TypeId args <- (fromTypeId -> args)
    where TypeId = toTypeId
pattern KindId :: (IdC id args) => args -> id KindLevel
pattern KindId args <- (fromKindId -> args)
    where KindId = toKindId




data SourceId level where
    SourceTermId :: String -> SourceId TermLevel
    SourceTypeId :: String -> SourceId TypeLevel
    SourceKindId :: String -> SourceId KindLevel

instance IdC SourceId String where
    toTermId = SourceTermId
    fromTermId (SourceTermId str) = str
    toTypeId = SourceTypeId
    fromTypeId (SourceTypeId str) = str
    toKindId = SourceKindId
    fromKindId (SourceKindId str) = str

instance Eq (SourceId level) where
    (SourceTermId x) == (SourceTermId y) = x == y
    (SourceTypeId a) == (SourceTypeId b) = a == b
    (SourceKindId k) == (SourceKindId k') = k == k'

instance Show (SourceId level) where
    show (SourceTermId x) = x
    show (SourceTypeId a) = '\'':a
    show (SourceKindId k) = "''"++k


data DeBruijnId orig level where
    DeBruijnTermId :: Int -> orig -> DeBruijnId orig TermLevel
    DeBruijnTypeId :: Int -> orig -> DeBruijnId orig TypeLevel
    DeBruijnKindId :: Int -> orig -> DeBruijnId orig KindLevel

instance IdC (DeBruijnId orig) (Int, orig) where
    toTermId = uncurry DeBruijnTermId
    fromTermId (DeBruijnTermId i str) = (i, str)
    toTypeId = uncurry DeBruijnTypeId
    fromTypeId (DeBruijnTypeId i str) = (i, str)
    toKindId = uncurry DeBruijnKindId
    fromKindId (DeBruijnKindId i str) = (i, str)

instance Eq (DeBruijnId orig level) where
    (DeBruijnTermId x _) == (DeBruijnTermId y _) = x == y
    (DeBruijnTypeId a _) == (DeBruijnTypeId b _) = a == b
    (DeBruijnKindId k _) == (DeBruijnKindId k' _) = k == k'
