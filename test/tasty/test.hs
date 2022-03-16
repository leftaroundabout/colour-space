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

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  defaultMain $ testGroup "Tests"
   [
   ]


infix 4 ≈≈, ≈≈≈

class NearEq a where
  (≈≈) :: a -> a -> Bool

instance (NearEq a, NearEq b) => NearEq (a,b) where
  (x,y) ≈≈ (ξ,υ) = x≈≈ξ && y≈≈υ
instance NearEq D¹ where
  D¹ p ≈≈ D¹ q = abs (p-q) < 1e-9

(≈≈≈) :: (NearEq a, Eq a, Show a)
      => a -> a -> QC.Property
p≈≈≈q
 | p≈≈q       = QC.property True
 | otherwise  = p===q


