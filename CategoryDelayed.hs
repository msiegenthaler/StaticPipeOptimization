{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module CategoryDelayed (
) where

import Control.Category


class Category cat => CategoryChain chain cat a b | chain -> cat a b


data CategoryBox cat a b where
    CategoryBox :: Category cat => cat a b -> CategoryBox cat a b
instance Category cat => CategoryChain (CategoryBox cat a b) cat a b

data CCNil = CCNil
instance Category cat => CategoryChain CCNil cat a b
instance Show CCNil where
    show _ = "CCNil"

data CCCons cat a b c tail where
    CCCons :: (Category cat, CategoryChain tail cat b c) =>
                    cat a b -> tail -> CCCons cat a b c tail
instance (Category cat) =>
            CategoryChain (CCCons cat a b c tail) cat a c

instance (Category cat, Show (cat a b), Show tail, CategoryChain tail cat a c, Show (cat a c)) =>
            Show (CCCons cat a b c tail) where
    show (CCCons e t) = "CCons " ++ show e ++ " (" ++ show t ++ ")"




xxx :: (Category cat, CategoryChain chain cat a b) => chain -> String
xxx _ = "ok"

yyy :: (CategoryChain chain MyFuns a b) => chain -> String
yyy _ = "ok"





data MyFuns i o = AddOne
                | AddTwo
                | Ident deriving (Show, Eq)
instance Category MyFuns where
    id = Ident
    a . b = undefined





{-

data CatNil cat a = CatNil
data CatCons cat a c t where
    CatCons :: (Category cat, CategoryDelayed t cat b c) => cat a b -> t -> CatCons h a c t


class Category cat => CategoryDelayed del cat a b
-- instance Category cat => CategoryDelayed (CatNil (cat a a) a) cat a a
instance (Category cat1, Category cat0) => CategoryDelayed (CatNil (cat1 a1 a1) a1) cat0 a0 a0
instance Category cat => CategoryDelayed (CatCons (cat a b) a c t) cat a c

instance Show (CatNil cat a) where
    show _ = "CatNil"
instance Show (CatCons cat a b t) where
    show _ = "CatCons"

(>>>) :: (Category cat, CategoryDelayed t cat b c) => cat a b -> t -> CatCons h a c t
(>>>) = CatCons


myCat :: String -> String
myCat i = i ++ "!"


xxx :: (Category cat, CategoryDelayed del cat a a) => del -> String
xxx _ = "ok"
-}