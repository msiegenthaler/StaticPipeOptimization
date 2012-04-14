{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances, OverlappingInstances #-}

module Main (
    main
) where

import OptPipe
import Pipe
import HNat

--Example usage: 5 $> AddOne |> AddTwo |> ShowAsString |> eop

--example elements
data Id = Id deriving Show
instance PipeElement Id a a where runPipe _ = id
data AddOne = AddOne deriving Show
instance Num a => PipeElement AddOne a a where runPipe _ = (+1)
data AddTwo = AddTwo deriving Show
instance Num a => PipeElement AddTwo a a where runPipe _ = (+2)
data AddThree = AddThree deriving Show
instance Num a => PipeElement AddThree a a where runPipe _ = (+3)
data ShowAsString = ShowAsString deriving Show
instance Show a => PipeElement ShowAsString a String where runPipe _ = show
data IntFromString = IntFromString deriving Show
instance PipeElement IntFromString String Int where runPipe _ = read


--optimizations
instance Num a => Optimizable AddOne AddOne AddTwo a a a HTrue where
    mergePipe _ _ = AddTwo
instance Num a => Optimizable AddOne AddTwo AddThree a a a HTrue where
    mergePipe _ _ = AddThree
instance Num a => Optimizable AddTwo AddOne AddThree a a a HTrue where
    mergePipe _ _ = AddThree


main = do
    print "Examples (should all print 10)"
    let p1 = AddOne |> AddTwo |> ShowAsString |> eop
    let e1 = 7 $> p1
    let e1' = 7 $$> p1
    print e1
    print e1'
    let p2 = AddOne |> AddOne |> AddOne |> AddThree |> eop
    let e2  = 4 $> p2
    let e2' = 4 $$> p2
    print e2
    print e2'


-- A simple example for handling of HNat
class Counter c where counter :: c -> IO ()
instance Counter HZero where counter _ = print "done"
instance (HNat n, Counter n) => Counter (HSucc n) where counter n = print ("tick " ++ (show n)) >> counter (hPred n)
