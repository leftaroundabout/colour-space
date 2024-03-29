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
import Data.Manifold.WithBoundary
import Data.VectorSpace
import Data.Semigroup

import Data.Colour.Manifold
import Data.Colour.Manifold.Internal
import Data.Colour
import Data.Colour.SRGB.Linear

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC
import System.Random (Random)
import Text.Printf

main :: IO ()
main = do
  putStrLn "here"
  defaultMain $ testGroup "Tests"
   [ testGroup "Sanity checks"
    [ testProperty "Self equality"
     $ \(c :: Colour ℝ) -> c ≈≈≈ c
    , testProperty "Equality nontriviality"
     . QC.expectFailure $ \(c :: Colour ℝ) d -> c ≈≈≈ d
    ]
   , testGroup "Manifold laws"
    [ testProperty "Zero displacement"
     $ \(c :: Colour ℝ) -> case toInterior c of
          Nothing -> undefined
          Just ci -> case c .+^| zeroV of
            Right c' -> c'≈≈≈ci
    , testProperty "Boundary retrieval"
     $ \ce -> separateInterior (fromBoundary ce :: Colour ℝ) ≈≈≈ Left ce
    , testProperty "Interior retrieval"
     $ \ci -> separateInterior (fromInterior ci :: Colour ℝ) ≈≈≈ Right ci
    , testProperty "Difference re-addition"
     $ \(c :: Colour ℝ) d
         -> let Just v = c.--.d
            in QC.counterexample (printf "v = %s\n" (show v)) $ case d .+^| v of
                   Left (cb, η) -> QC.property (abs η < 1e-6) .&&. fromBoundary cb ≈≈≈ c
                   Right ci     -> fromInterior ci ≈≈≈ c
    , testProperty "Displacement re-subtraction"
     $ \(c :: Colour ℝ) v
         -> case c .+^| v of
              Left (db, η) -> let w = fromBoundary db.--!c
                                  μ = v<.>w
                                  λ = v<.>v
                              in QC.property (μ>=0 && μ<=λ)
                                     .&&. v^*(μ/λ) ≈≈≈ w
                                     .&&. w^*(1+η) ≈≈≈ v
              Right di     -> fromInterior di.--!c ≈≈≈ v
    ]
   ]


infix 4 ≈≈, ≈≈≈

class NearEq a where
  (≈≈) :: a -> a -> Bool

instance NearEq ℝ where
  n ≈≈ m
   | abs n<=1, m<=1  = abs (n-m) < 1e-9
   | otherwise       = abs (n-m) < 1e-9 * (abs n + abs n)
instance (NearEq a, NearEq b) => NearEq (a,b) where
  (x,y) ≈≈ (ξ,υ) = x≈≈ξ && y≈≈υ
instance (NearEq a, NearEq b, NearEq c) => NearEq (a,b,c) where
  (x,y,z) ≈≈ (ξ,υ,ζ) = x≈≈ξ && y≈≈υ && z≈≈ζ
instance NearEq a => NearEq (RGB a) where
  RGB r g b ≈≈ RGB ρ γ β = r≈≈ρ && g≈≈γ && b≈≈β
instance NearEq a => NearEq (Maybe a) where
  Just p ≈≈ Just q = p≈≈q
  Nothing ≈≈ Nothing = True
  _ ≈≈ _ = False
instance (NearEq a, NearEq b) => NearEq (Either a b) where
  Left p ≈≈ Left q = p≈≈q
  Right p ≈≈ Right q = p≈≈q
  _ ≈≈ _ = False
instance NearEq D¹ where
  D¹ p ≈≈ D¹ q = abs (p-q) < 1e-9
instance NearEq S² where
  p ≈≈ q = magnitude (p.-~.q) < 1e-9

instance (NearEq a, Fractional a) => NearEq (Colour a) where
  c ≈≈ ζ = toRGB c ≈≈ toRGB ζ
instance NearEq ColourNeedle where
  ColourNeedle v ≈≈ ColourNeedle w = v ≈≈ w
instance (Fractional a, Random a) => QC.Arbitrary (Colour a) where
  arbitrary = rgb <$> QC.choose (0,1) <*> QC.choose (0,1) <*> QC.choose (0,1)
instance NearEq ColourBoundary where
  ColourBoundarySphere v ≈≈ ColourBoundarySphere w = v ≈≈ w

(≈≈≈) :: (NearEq a, Eq a, Show a)
      => a -> a -> QC.Property
p≈≈≈q
 | p≈≈q       = QC.property True
 | otherwise  = p===q




instance InnerSpace ColourNeedle where
  ColourNeedle c <.> ColourNeedle d = pr+pg+pb
   where RGB pr pg pb = (*)<$>c<*>d
