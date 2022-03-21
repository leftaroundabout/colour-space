-- |
-- Module      : test
-- Copyright   : (c) Justus Sagemüller 2022
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE CPP                 #-}

import Math.Manifold.Core.PseudoAffine
import Data.Manifold.Types

import Data.Colour.Manifold
import Data.Colour
import Data.Colour.SRGB.Linear

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC
import System.Random (Random)

main :: IO ()
main = do
  putStrLn "here"
  defaultMain $ testGroup "Tests"
   [ testGroup "Sanity checks"
    [ testProperty "Self equality"
     $ \(c :: Colour ℝ) -> c ≈≈ c
    , testProperty "Equality nontriviality"
     . QC.expectFailure $ \(c :: Colour ℝ) d -> c ≈≈ d
    ]
   , testGroup "Manifold laws"
    [  ]
   ]


infix 4 ≈≈, ≈≈≈

class NearEq a where
  (≈≈) :: a -> a -> Bool

instance NearEq ℝ where
  n ≈≈ m
   | n>=0, m>=0, n<=1, m<=1  = abs (n-m) < 1e-9
instance (NearEq a, NearEq b) => NearEq (a,b) where
  (x,y) ≈≈ (ξ,υ) = x≈≈ξ && y≈≈υ
instance (NearEq a, NearEq b, NearEq c) => NearEq (a,b,c) where
  (x,y,z) ≈≈ (ξ,υ,ζ) = x≈≈ξ && y≈≈υ && z≈≈ζ
instance NearEq a => NearEq (RGB a) where
  RGB r g b ≈≈ RGB ρ γ β = r≈≈ρ && g≈≈γ && b≈≈β
instance NearEq D¹ where
  D¹ p ≈≈ D¹ q = abs (p-q) < 1e-9

instance (NearEq a, Fractional a) => NearEq (Colour a) where
  c ≈≈ ζ = toRGB c ≈≈ toRGB ζ
instance (Fractional a, Random a) => QC.Arbitrary (Colour a) where
  arbitrary = rgb <$> QC.choose (0,1) <*> QC.choose (0,1) <*> QC.choose (0,1)

(≈≈≈) :: (NearEq a, Eq a, Show a)
      => a -> a -> QC.Property
p≈≈≈q
 | p≈≈q       = QC.property True
 | otherwise  = p===q


