{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances, OverlappingInstances #-}

module Main (
    main
) where

import OptPipe
import Pipe
import HNat

--Example usage: 5 $> AddOne |> AddTwo |> ShowAsString

--example elements
data Id = Id deriving Show
instance PipeElement Id a a where runPipe _ = id
data AddOne = AddOne deriving Show
instance Num a => PipeElement AddOne a a where runPipe _ = (+1)
data AddTwo = AddTwo deriving Show
instance Num a => PipeElement AddTwo a a where runPipe _ = (+2)
data AddThree = AddThree deriving Show
instance Num a => PipeElement AddThree a a where runPipe _ = (+3)
data AddAny a = AddAny a deriving Show
instance Num a => PipeElement (AddAny a) a a where runPipe (AddAny n) = (+n)
data ShowAsString = ShowAsString deriving Show
instance Show a => PipeElement ShowAsString a String where runPipe _ = show
data IntFromString = IntFromString deriving Show
instance PipeElement IntFromString String Int where runPipe _ = read


--optimizations

instance Optimizable AddOne AddOne AddTwo HTrue where
    mergePipe _ _ = AddTwo

instance Optimizable AddOne AddTwo AddThree HTrue where
    mergePipe _ _ = AddThree
instance Optimizable AddTwo AddOne AddThree HTrue where
    mergePipe _ _ = AddThree

instance Num a => Optimizable AddTwo AddTwo (AddAny a) HTrue where
    mergePipe _ _ = AddAny 4
instance Num a => Optimizable AddOne AddThree (AddAny a) HTrue where
    mergePipe _ _ = AddAny 4
instance Num a => Optimizable AddThree AddOne (AddAny a) HTrue where
    mergePipe _ _ = AddAny 4

instance Num a => Optimizable (AddAny a) AddOne (AddAny a) HTrue where
    mergePipe (AddAny a) _ = AddAny (a + 1)
instance Num a => Optimizable (AddAny a) AddTwo (AddAny a) HTrue where
    mergePipe (AddAny a) _ = AddAny (a + 2)
instance Num a => Optimizable (AddAny a) AddThree (AddAny a) HTrue where
    mergePipe (AddAny a) _ = AddAny (a + 3)
instance Num a => Optimizable AddOne (AddAny a) (AddAny a) HTrue where
    mergePipe _ (AddAny a) = AddAny (a + 1)
instance Num a => Optimizable AddTwo (AddAny a) (AddAny a) HTrue where
    mergePipe _ (AddAny a) = AddAny (a + 2)
instance Num a => Optimizable AddThree (AddAny a) (AddAny a) HTrue where
    mergePipe _ (AddAny a) = AddAny (a + 3)

instance Num a => Optimizable (AddAny a) (AddAny a) (AddAny a) HTrue where
    mergePipe (AddAny a) (AddAny b) = AddAny (a + b)

instance Optimizable ShowAsString IntFromString Id HTrue where
    mergePipe _ _ = Id

instance PipeElement x a a => Optimizable Id x x HTrue where
    mergePipe _ x = x
instance PipeElement x a a => Optimizable x Id x HTrue where
    mergePipe x _ = x

-- end of optimizations


main = do
    print "Examples (should all print 10)"
    let p1 = AddOne |> AddTwo |> ShowAsString
    let e1 = 7 $> p1
    let e1' = 7 $$> p1
    print e1
    print e1'
    let p2 = AddOne |> AddOne |> AddOne |> AddThree
    let e2  = 4 $> p2
    let e2' = 4 $$> p2
    print e2
    print e2'
    let p3 = AddOne |> ShowAsString |> IntFromString |> AddTwo
    let e3 = 7 $> p3
    let e3' = 7 $$> p3
    print e3
    print e3'


-- A simple example for handling of HNat
class Counter c where counter :: c -> IO ()
instance Counter HZero where counter _ = print "done"
instance (HNat n, Counter n) => Counter (HSucc n) where counter n = print ("tick " ++ (show n)) >> counter (hPred n)
