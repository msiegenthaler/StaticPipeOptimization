{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances, OverlappingInstances, GADTs #-}

module Pipe (
    PipeElement(..),
    CompPipe,
    (|>),
    ($>),
    CPCons(..),
    CPNil
) where

import HBool
import TypeEq


class PipeElement e i o | e -> i o where
    runPipe :: e -> i -> o --simplified for the sake of the example

data CPCons e i o l where
    CPCons :: (PipeElement e i x, CompPipe l x o) => e -> l -> CPCons e i x l
data CPNil = CPNil

instance (CompPipe l x o) => PipeElement (CPCons e i x l) i o where
    runPipe = runCompPipe
instance PipeElement CPNil a a where
    runPipe _ = id

class CompPipe e i o | e -> i o where
    runCompPipe :: e -> i -> o
instance CompPipe CPNil a a where
    runCompPipe _ = id
instance (CompPipe l x o) => CompPipe (CPCons  e i x l) i o where
    runCompPipe (CPCons e l) = runCompPipe l . runPipe e

instance (Show e, Show l) => Show (CPCons e a b l) where show (CPCons e l) = "CPCons " ++ show e ++ " (" ++ show l ++ ")"
instance Show CPNil where show _ = "CPNil"


class (PipeElement p i x, PipeElement p' x o, PipeElement p'' i o) =>
        AppendPipe p p' p'' i x o | p p' -> p'' i x o where
    appendPipe :: p -> p' -> p''
instance (AppendPipeCase flag p p' p'', IsCompPipe p flag, PipeElement p i x, PipeElement p' x o,
            PipeElement p'' i o) =>
        AppendPipe p p' p'' i x o where
    appendPipe = appendPipeCase (undefined::flag)

class AppendPipeCase flag p p' p'' | flag p p' -> p'' where
    appendPipeCase :: flag -> p -> p' -> p''
instance (PipeElement p' i o) => AppendPipeCase HTrue CPNil p' p' where
    appendPipeCase _ _ = id
instance (AppendPipe l p l' x x' o, CompPipe l' x o) =>
        AppendPipeCase HTrue (CPCons e i x l) p (CPCons e i x l') where
    appendPipeCase _ (CPCons e l) p = CPCons e (appendPipe l p)
instance (PipeElement p i x, PipeElement p' x o) =>
        AppendPipeCase HFalse p p' (CPCons p i x (CPCons p' x o CPNil)) where
    appendPipeCase _ p p' = CPCons p $ CPCons p' CPNil


class HBool b => IsCompPipe p b | p -> b
instance IsCompPipe CPNil HTrue
instance IsCompPipe (CPCons e i o l) HTrue
instance (HBool flag, TypeCast flag HFalse) => IsCompPipe e flag



-- | concats the two PipeElements into one
(|>) :: (AppendPipe l l' l'' i x o) => l -> l' -> l''
(|>) = appendPipe
infixr 3 |>


-- | Runs the pipe (input $> pipe)
($>) i e = runPipe e i
infixr 1 $>
