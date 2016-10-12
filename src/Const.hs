module Const where

class Unit c where
    mkUnit :: c
    isUnit :: c -> Bool

class Arr c where
    mkArr :: c
    isArr :: c -> Bool