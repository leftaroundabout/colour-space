{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE CPP                  #-}

module Data.Colour.Manifold.Internal where

import Data.Colour.SRGB.Linear
import Data.Manifold.Types
import Math.LinearMap.Category
import Linear.V3
import GHC.Generics

newtype ColourNeedle = ColourNeedle { getRGBNeedle :: RGB ℝ } deriving (Eq, Show)

asV3Needle :: ColourNeedle -+> V3 ℝ
asV3Needle = LinearFunction $ \(ColourNeedle (RGB r g b)) -> V3 r g b

fromV3Needle :: V3 ℝ -+> ColourNeedle
fromV3Needle = LinearFunction $ \(V3 r g b) -> ColourNeedle $ RGB r g b


newtype ColourBoundary = ColourBoundarySphere {
   getColourBounarySphere :: S² -- ^ Corresponds to an inflated version of the HSL bicone
  }
 deriving (Generic, Semimanifold, PseudoAffine)

data ColourHalfNeedle = ColourHalfNeedle {
         colourBoundaryDistance :: !ℝay
       , colourBoundaryTangent :: !(Needle ColourBoundary)
       }
   deriving (Generic)
