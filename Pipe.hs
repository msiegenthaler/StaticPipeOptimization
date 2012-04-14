{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GADTs #-}

module Pipe (
    PipeElement(..),
    CompPipe,
    (|>),
    ($>),
    eop,
    CPCons(..),
    CPNil
) where

class PipeElement e i o | e -> i o where
    runPipe :: e -> i -> o --simplified for the sake of the example


class PipeElement e i o => CompPipe e i o

data CPCons e i o l where
    CPCons :: (PipeElement e i x, CompPipe l x o) => e -> l -> CPCons e i x l
data CPNil = CPNil

instance CompPipe CPNil a a
instance (CompPipe l x o) => CompPipe (CPCons  e i x l) i o
instance (CompPipe l x o) => PipeElement (CPCons e i x l) i o where
    runPipe (CPCons e l) = runPipe l . runPipe e
instance PipeElement CPNil a a where
    runPipe e = id
instance (Show e, Show l) => Show (CPCons e a b l) where show (CPCons e l) = "CPCons " ++ show e ++ " " ++ show l
instance Show CPNil where show _ = "CPNil"


eop = CPNil

(|>) :: (PipeElement e i o, CompPipe l o x) => e -> l -> CPCons e i o l
(|>) = CPCons
infixr 2 |>

-- | Runs the pipe (input $> pipe)
($>) i e = runPipe e i
infixr 1 $>
