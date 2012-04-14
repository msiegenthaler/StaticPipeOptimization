module HBool (
    HTrue,
    HFalse,
    HBool
) where

data HTrue
data HFalse

class HBool b where hBool :: b -> Bool
instance HBool HTrue where hBool _ = True
instance HBool HFalse where hBool _ = False