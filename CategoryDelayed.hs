{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}

module CategoryDelayed (
    (>>>*),
    applyChain
) where

import qualified Prelude
import Prelude hiding (id)
import Control.Category
import TypeEq
import HBool

-- | not yet applied sequence of category composition (c1 >>> c2 >>> c3)
class Category cat => CategoryChain chain cat a b | chain -> cat a b where
    -- | Simply executes the delayed composition without applying any optimizations.
    -- | The same as if >>> had been used instead of >>>* for the composition
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


class (Category cat, CategoryChain tail cat b c) =>
        Chainable ca cat a b c tail | ca -> cat a b tail where
    toChain :: ca -> CCCons cat a b c tail
instance (ChainableCase flag ca cat a b c tail, IsCategoryChain ca flag, Category cat,
            CategoryChain tail cat b c) =>
        Chainable ca cat a b c tail where
    toChain = toChainCase (undefined::flag)

class (HBool flag) => ChainableCase flag ca cat a b c tail | flag ca -> cat a b c tail where
    toChainCase :: flag -> ca -> CCCons cat a b c tail
instance ChainableCase HTrue (CCCons cat a b c tail) cat a b c tail where
    toChainCase _ = Prelude.id
instance Category cat => ChainableCase HFalse (cat a b) cat a b b CCNil where
    toChainCase _ e = CCCons e CCNil

class HBool b => IsCategoryChain chain b | chain -> b
instance IsCategoryChain (CCCons cat a b c tail) HTrue
instance IsCategoryChain CCNil HTrue
instance (HBool flag, TypeCast flag HFalse) => IsCategoryChain e flag


-- | Left-to-Right composition of category. Basically the same as >>>, but the actual composition
-- | is delayed so that optimizations can be applied.
(>>>*) e t = CCCons e $ toChain t
infixr 1 >>>*


