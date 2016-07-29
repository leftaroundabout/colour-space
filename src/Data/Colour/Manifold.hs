{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Colour.Manifold (Colour, QuantisedColour(..)) where

import Control.Applicative
import Control.Arrow
import Data.Semigroup

import Data.Manifold.PseudoAffine
import Data.Manifold.Types
import Data.Manifold.Riemannian
import Data.VectorSpace
import Data.AdditiveGroup

import Data.Colour.SRGB (toSRGB, toSRGB24)
import Data.Colour.SRGB.Linear
import Data.Colour

import Codec.Picture.Types

newtype ColourNeedle = ColourNeedle { getRGBNeedle :: RGB ℝ }

withRGBNeedle :: (RGB Double -> RGB Double) -> ColourNeedle -> ColourNeedle
withRGBNeedle f (ColourNeedle q) = ColourNeedle $ f q

instance AdditiveGroup ColourNeedle where
  zeroV = ColourNeedle $ RGB 0 0 0
  negateV = withRGBNeedle $ fmap negate
  ColourNeedle q ^+^ ColourNeedle s = ColourNeedle $ liftA2 (+) q s
instance VectorSpace ColourNeedle where
  type Scalar ColourNeedle = ℝ
  (*^)μ = withRGBNeedle $ fmap (μ*)

instance Semimanifold ColourNeedle where
  type Needle ColourNeedle = ColourNeedle
  fromInterior = id; toInterior = pure
  translateP = pure (^+^)

instance PseudoAffine ColourNeedle where
  ColourNeedle q .-~. ColourNeedle s = pure . ColourNeedle $ liftA2 (-) q s


fromLtdRGB :: LtdCol -> Colour ℝ
fromLtdRGB = fmap (\(CD¹ h Origin) -> h) >>> \(RGB r g b) -> rgb r g b

toLtdRGB :: Colour ℝ -> LtdCol
toLtdRGB = toRGB >>> fmap ((`CD¹`Origin) . min 1 . max 0)

type LtdCol = RGB (CD¹ ℝ⁰)

bijectToLtd :: ℝ -> CD¹ ℝ⁰
bijectToLtd y = CD¹ ( ( y - 1 + sqrt(1+y^2) ) / (2*y) ) Origin
-- y = (x - 1/2) / (x*(1 - x))
-- y * x * (1 - x) = x - 1/2
-- y * x² - (1 - y) * x - 1/2 = 0
-- y * x² + (y - 1) * x - 1/2 = 0
-- x = (1 - y ± sqrt( (1-y)² + 2*y ) ) / (-2*y)
--   = (y - 1 +! sqrt( 1 + y² ) ) / (2*y)

bijectFromLtd :: CD¹ ℝ⁰ -> Option ℝ
bijectFromLtd (CD¹ x Origin)
    | x>0 && x<1  = return $ (x - 0.5) / (x*(1 - x))
    | otherwise   = empty

instance Semimanifold (Colour ℝ) where
  type Interior (Colour ℝ) = ColourNeedle
  type Needle (Colour ℝ) = ColourNeedle
  fromInterior (ColourNeedle q) = fromLtdRGB $ fmap bijectToLtd q
  toInterior = fmap ColourNeedle . toin . toLtdRGB
   where toin (RGB r g b) = liftA3 RGB (bijectFromLtd r) (bijectFromLtd g) (bijectFromLtd b)
  translateP = pure (^+^)

instance PseudoAffine (Colour ℝ) where
  c .-~. ζ = (^-^ζ) <$> toInterior c

instance Geodesic (Colour ℝ) where
  geodesicBetween a b = pure $ \(D¹ q) -> blend ((q+1)/2) a b


class QuantisedColour c where
  quantiseColour :: Colour ℝ -> c

instance QuantisedColour PixelRGBF where
  quantiseColour c = PixelRGBF r g b
   where RGB r g b = fmap realToFrac $ toSRGB c
  
instance QuantisedColour PixelRGB8 where
  quantiseColour c = PixelRGB8 r g b
   where RGB r g b = toSRGB24 c

