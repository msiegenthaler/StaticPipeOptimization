Show the possibility to do static optimization of pipe-like constructs in haskell.

Based upon the concepts introduced in the HList paper (http://homepages.cwi.nl/~ralf/HList/paper.pdf) such as TypeCast and OccursMay.


### Example ###
It includes an example with simple arithmetics (AddOne, AddTwo etc.). See Main.hs.
Example usage: 

```haskell
5 $> AddOne |> AddTwo |> ShowAsString  --normal execution
5 $$> AddOne |> AddTwo |> ShowAsString --optimized
optimize (AddOne |> AddOne)
```

Optimizations are declared as follows:

````haskell
instance Num a => Optimizable AddOne AddTwo AddThree a a a HTrue where
    mergePipe _ _ = AddThree
````
