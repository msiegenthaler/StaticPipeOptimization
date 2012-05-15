{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module CategoryDelayed (
) where

import Prelude hiding (id)
import Control.Category


class Category cat => CategoryChain chain cat a b | chain -> cat a b where
    applyChain :: chain -> cat a b

data CCNil = CCNil
instance Category cat => CategoryChain CCNil cat a a where
    applyChain _ = id
instance Show CCNil where
    show _ = "CCNil"

data CCCons cat a b c tail where
    CCCons :: (Category cat, CategoryChain tail cat b c) => cat a b -> tail -> CCCons cat a b c tail
instance (Category cat) =>
            CategoryChain (CCCons cat a b c tail) cat a c where
    applyChain (CCCons e t) = e >>> applyChain t
instance (CategoryChain tail cat a c, Show tail, Show (cat a b)) =>
            Show (CCCons cat a b c tail) where
    show (CCCons e t) = "CCCons " ++ show e ++ " (" ++ show t ++ ")"


(*>>>) :: (Category cat, CategoryChain tail cat b c) => cat a b -> tail -> CCCons cat a b c tail
(*>>>) = CCCons
infixr 1 *>>>



--- Example Category
data MyFuns i o where
    AddOne :: MyFuns Int Int
    AddTwo :: MyFuns Int Int
    AddX   :: Int -> MyFuns Int Int
    Ident  :: MyFuns a a
instance Category MyFuns where
    id = Ident
    Ident . o = o
    o . Ident = o
    AddOne . AddOne = AddTwo
    AddOne . AddTwo = AddX 3
    AddTwo . AddOne = AddX 3
    AddTwo . AddTwo = AddX 4
instance Show (MyFuns a b) where
    show Ident = "Ident"
    show AddOne = "AddOne"
    show AddTwo = "AddTwo"
    show (AddX x) = "AddX " ++ show x