{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module TypeEq (
    TypeCast,
    TypeEq
) where

import HBool

class TypeCast a b | a -> b, b -> a where typeCast :: a -> b
class TypeCast' t a b | t a -> b, t b -> a where typeCast' :: t -> a -> b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t -> a -> b
instance TypeCast' () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x = x

class TypeEq a b flag | a b -> flag where typeEq :: flag
instance TypeEq x x HTrue where typeEq = undefined
instance (TypeCast HFalse flag) => TypeEq x y flag where typeEq = undefined
