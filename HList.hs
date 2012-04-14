module HList (
	HList,
	HNil,
	HCons,
	(.*.)
) where

class HList l

data HNil = HNil deriving (Eq,Show)
data HCons e l = HCons e l deriving (Eq,Show)

instance HList HNil
instance HList l => HList (HCons e l)
(.*.) :: (HList l) => e -> l -> HCons e l
(.*.) = HCons
infixr 1 .*.
