{-# LANGUAGE GADTs #-}

module CategoryExample (
) where

import CategoryDelayed
import Control.Category


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
