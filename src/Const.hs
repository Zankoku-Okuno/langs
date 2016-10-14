module Const where

class UnitC c where
    mkUnit :: c
    isUnit :: c -> Bool

class ArrC c where
    mkArr :: c
    isArr :: c -> Bool