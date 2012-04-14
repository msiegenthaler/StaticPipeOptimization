{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module HNat (
    HNat(..),
    HZero,
    HSucc,
    hSucc,
    hPred,
    -- * Type Operations
    HLessThan,
    HGreaterThan,
    HEqual,
    -- * Numbers
    H0, h0,
    H1, h1,
    H2, h2,
    H3, h3,
    H4, h4,
    H5, h5,
    H6, h6,
    H7, h7,
    H8, h8,
    H9, h9,
    H10, h10
) where

import HBool


data HZero
data HSucc n

hZero :: HZero
hZero = undefined
hSucc :: HNat n => n -> HSucc n
hSucc _ = undefined
hPred :: HNat n => HSucc n -> n
hPred _ = undefined

class HNat n where hNatToIntegral :: n -> Integer
instance HNat HZero where hNatToIntegral _ = 0
instance HNat n => HNat (HSucc n) where
    hNatToIntegral n = 1 + hNatToIntegral (hPred n)

instance Show HZero where show = ("h" ++) . show . hNatToIntegral
instance HNat n => Show (HSucc n) where show = ("h" ++) . show . hNatToIntegral


-- | Sets result to HTrue if a is less than b and HFalse otherwise
class HBool result => HLessThan a b result | a b -> result
instance HLessThan HZero HZero HFalse
instance HNat n => HLessThan HZero (HSucc n) HTrue
instance HNat n => HLessThan (HSucc n) HZero HFalse
instance HLessThan a b result => HLessThan (HSucc a) (HSucc b) result

-- | Sets result to HTrue if a is greated than b and HFalse otherwise
class HGreaterThan a b result | a b -> result
instance (HLessThan a b res, HNegate res neg) => HGreaterThan a b neg

-- | Sets result to HTrue if a and b are the same and HFalse otherwise
class HEqual a b result | a b -> result
instance HEqual HZero HZero HTrue
instance HEqual HZero (HSucc a) HFalse
instance HEqual (HSucc a) HZero HFalse
instance HEqual a b result => HEqual (HSucc a) (HSucc b) result


-- Some Numbers...

type H0 = HZero
type H1 = HSucc H0
type H2 = HSucc H1
type H3 = HSucc H2
type H4 = HSucc H3
type H5 = HSucc H4
type H6 = HSucc H5
type H7 = HSucc H6
type H8 = HSucc H7
type H9 = HSucc H8
type H10 = HSucc H9

h0 :: H0
h0 = undefined
h1 :: H1
h1 = undefined
h2 :: H2
h2 = undefined
h3 :: H3
h3 = undefined
h4 :: H4
h4 = undefined
h5 :: H5
h5 = undefined
h6 :: H6
h6 = undefined
h7 :: H7
h7 = undefined
h8 :: H8
h8 = undefined
h9 :: H9
h9 = undefined
h10 :: H10
h10 = undefined
