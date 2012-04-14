{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module HBool (
    HTrue,
    HFalse,
    HBool,
    HNegate
) where

data HTrue
data HFalse

class HBool b where hBool :: b -> Bool
instance HBool HTrue where hBool _ = True
instance HBool HFalse where hBool _ = False

class HBool b => HNegate b result | b -> result
instance HNegate HTrue HFalse
instance HNegate HFalse HTrue
