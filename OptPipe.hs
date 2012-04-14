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

-- | Optimize the pipe
optimize :: (PipeElement p i o, PipeElement p' i o, LoopOpt p p' i o) => p -> p'
optimize = loopOpt

-- | Run the pipe with optimization (input $$> pipe)
($$>) :: (PipeElement p i o, PipeElement p' i o, LoopOpt p p' i o) => i -> p -> o
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


-- Repeatedly applies the optimization (optPipe) until no more optimization is possible
class (PipeElement p i o, PipeElement p' i o) => LoopOpt p p' i o | p -> i o p' where
    loopOpt :: p -> p'
instance (PipeElement p i o, PipeElement p' i o, IsOptimizable p flag, LoopOptCase flag p p' i o) =>
            LoopOpt p p' i o where
    loopOpt = loopOptCase (undefined::flag)
class LoopOptCase flag p p' i o | flag p -> i o p' where
    loopOptCase :: flag -> p -> p'
instance (LoopOpt p' p'' i o, OptPipe p p') => LoopOptCase HTrue p p'' i o where
    loopOptCase _ p = loopOpt $ optPipe p
instance LoopOptCase HFalse p p i o where
    loopOptCase _ = id

class IsOptimizable p result | p -> result
instance IsOptimizable CPNil HFalse
instance IsOptimizable (CPCons e i o CPNil) HFalse
instance (HOr flag1 flag2 result, IsOptimizable (CPCons e2 x o p) flag1, Optimizable e1 e2 e' i x o flag2) => 
            IsOptimizable (CPCons e1 i x (CPCons e2 x o p)) result
instance TypeCast flag HFalse => IsOptimizable other flag


class OptPipe p p' | p -> p' where
    optPipe :: p -> p'
instance OptPipe CPNil CPNil where
    optPipe = id
instance OptPipe (CPCons e i o CPNil) (CPCons e i o CPNil) where
    optPipe = id
instance (CompPipe p x2 o, Optimizable e1 e2 e' i x1 x2 flag, OptPipeCase flag e1 e2 p p') =>
        OptPipe (CPCons e1 i x1 (CPCons e2 x1 x2 p)) p' where
    optPipe (CPCons e1 (CPCons e2 p)) = optPipeCase (undefined::flag) e1 e2 p

class OptPipeCase flag e1 e2 p p' | flag e1 e2 p -> p' where
    optPipeCase :: flag -> e1 -> e2 -> p -> p'
--optimization
instance (CompPipe p x2 o, PipeElement e1 i x1, PipeElement e2 x1 x2, PipeElement e' i x2, 
          Optimizable e1 e2 e' i x1 x2 HTrue, OptPipe (CPCons e' i x2 p) p') =>
        OptPipeCase HTrue e1 e2 p p' where
    optPipeCase _ e1 e2 p = optPipe $ CPCons (mergePipe e1 e2) p
--no optimization
instance (CompPipe p x2 o, PipeElement e1 i x1, PipeElement e2 x1 x2,
          OptPipe (CPCons e2 x1 x2 p) p', CompPipe p' x1 o) =>
        OptPipeCase HFalse e1 e2 p (CPCons e1 i x1 p') where
    optPipeCase _ e1 e2 p = CPCons e1 $ optPipe $ CPCons e2 p
