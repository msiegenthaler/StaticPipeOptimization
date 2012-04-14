{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module HBool (
    HTrue,
    HFalse,
    HBool,
    -- * Operations
    HNegate,
    HAnd,
    HOr
) where

data HTrue
data HFalse

class HBool b where hBool :: b -> Bool
instance HBool HTrue where hBool _ = True
instance HBool HFalse where hBool _ = False

instance Show HTrue where show _ = "hTrue"
instance Show HFalse where show _ = "hFalse"


class HBool b => HNegate b result | b -> result
instance HNegate HTrue HFalse
instance HNegate HFalse HTrue

class (HBool a, HBool b, HBool result) => HAnd a b result | a b -> result
instance HAnd HTrue HTrue HTrue
instance HAnd HFalse HTrue HFalse
instance HAnd HTrue HFalse HFalse
instance HAnd HFalse HFalse HFalse

class (HBool a, HBool b, HBool result) => HOr a b result | a b -> result
instance HOr HTrue HTrue HTrue
instance HOr HFalse HTrue HTrue
instance HOr HTrue HFalse HTrue
instance HOr HFalse HFalse HFalse

