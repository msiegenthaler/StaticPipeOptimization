{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances, OverlappingInstances #-}

module OptPipe (
    ($$>),
    optimize,
    Optimizable(..),
    HTrue
) where

import Pipe
import HBool
import TypeEq

-- | Run the pipe with optimization (input $$> pipe)
($$>) i e = runPipe (optimize e) i
infixr 1 $$>

-- | Implement instances to allow for optimizations. b must be set to HTrue. 
class (PipeElement a i x, PipeElement b x o) =>
        Optimizable a b c i x o flag | a b -> i x o c flag where
    mergePipe :: a -> b -> c
--Fallback
instance (PipeElement a i x, PipeElement b x o, TypeCast flag HFalse) =>
        Optimizable a b c i x o flag where
    mergePipe = undefined



class (CompPipe p i o) => OptPipe p p' i o | p -> p' i o where
    optimize :: p -> p'
instance OptPipe CPNil CPNil a a where
    optimize = id
instance OptPipe (CPCons e i o CPNil) (CPCons e i o CPNil) i o where
    optimize = id
instance (CompPipe p x2 o, Optimizable e1 e2 e' i x1 x2 flag, OptPipeCase flag e1 e2 p p' i o) => 
        OptPipe (CPCons e1 i x1 (CPCons e2 x1 x2 p)) p' i o where
    optimize (CPCons e1 (CPCons e2 p)) = optimizeCase (undefined::flag) e1 e2 p

class OptPipeCase flag e1 e2 p p' i o | flag e1 e2 p -> p' i o where
    optimizeCase :: flag -> e1 -> e2 -> p -> p'
--optimization
instance (CompPipe p x2 o, PipeElement e1 i x1, PipeElement e2 x1 x2, PipeElement e' i x2, 
          Optimizable e1 e2 e' i x1 x2 HTrue, OptPipe (CPCons e' i x2 p) p' i o) =>
        OptPipeCase HTrue e1 e2 p p' i o where
    optimizeCase _ e1 e2 p = optimize $ CPCons (mergePipe e1 e2) p
--no optimization
instance (CompPipe p x2 o, PipeElement e1 i x1, PipeElement e2 x1 x2,
          OptPipe (CPCons e2 x1 x2 p) p' x1 o, CompPipe p' x1 o) =>
        OptPipeCase HFalse e1 e2 p (CPCons e1 i x1 p') i o where
    optimizeCase _ e1 e2 p = CPCons e1 $ optimize $ CPCons e2 p
