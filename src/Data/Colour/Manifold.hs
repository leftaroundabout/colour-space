{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Colour.Manifold (Colour, QuantisedColour(..)) where

import Control.Applicative (empty)
import Control.Applicative.Constrained
import Control.Arrow.Constrained
import Data.Semigroup

import Data.Manifold.PseudoAffine
import Data.Manifold.Types
import Data.Manifold.Riemannian
import Data.VectorSpace
import Data.AdditiveGroup

import Data.Colour.SRGB (toSRGB, toSRGB24)
import Data.Colour.SRGB.Linear
import Data.Colour

import Math.LinearMap.Category
import Linear.V3

import qualified Prelude as Hask
import Control.Category.Constrained.Prelude

import Codec.Picture.Types

import Data.Type.Coercion

newtype ColourNeedle = ColourNeedle { getRGBNeedle :: RGB ℝ }

asV3Needle :: ColourNeedle -+> V3 ℝ
asV3Needle = LinearFunction $ \(ColourNeedle (RGB r g b)) -> V3 r g b

fromV3Needle :: V3 ℝ -+> ColourNeedle
fromV3Needle = LinearFunction $ \(V3 r g b) -> ColourNeedle $ RGB r g b

asV3Tensor :: (ColourNeedle⊗w) -+> (V3 ℝ⊗w)
asV3Tensor = LinearFunction $ \(Tensor (RGB r g b)) -> Tensor $ V3 r g b

fromV3Tensor :: (V3 ℝ⊗w) -+> (ColourNeedle⊗w)
fromV3Tensor = LinearFunction $ \(Tensor (V3 r g b)) -> Tensor $ RGB r g b

withRGBNeedle :: (RGB Double -> RGB Double) -> ColourNeedle -> ColourNeedle
withRGBNeedle f (ColourNeedle q) = ColourNeedle $ f q

instance AdditiveGroup ColourNeedle where
  zeroV = ColourNeedle $ RGB 0 0 0
  negateV = withRGBNeedle $ fmap negate
  ColourNeedle q ^+^ ColourNeedle s = ColourNeedle $ liftA2 (+) q s
instance VectorSpace ColourNeedle where
  type Scalar ColourNeedle = ℝ
  (*^)μ = withRGBNeedle $ fmap (μ*)

instance TensorSpace ColourNeedle where
  type TensorProduct ColourNeedle w = RGB w
  zeroTensor = Tensor (RGB zeroV zeroV zeroV)
  toFlatTensor = LinearFunction $ \(ColourNeedle (RGB r g b)) -> Tensor (RGB r g b)
  fromFlatTensor = LinearFunction $ \(Tensor (RGB r g b)) -> ColourNeedle (RGB r g b)
  addTensors (Tensor (RGB r g b)) (Tensor (RGB r' g' b'))
                = Tensor $ RGB (r^+^r') (g^+^g') (b^+^b')
  negateTensor = LinearFunction $ \(Tensor (RGB r g b))
                       -> Tensor (RGB (negateV r) (negateV g) (negateV b))
  scaleTensor = bilinearFunction $ \μ (Tensor (RGB r g b))
                       -> Tensor (RGB (μ*^r) (μ*^g) (μ*^b))
  tensorProduct = bilinearFunction $ \(ColourNeedle (RGB r g b)) w
                       -> Tensor (RGB (r*^w) (g*^w) (b*^w))
  transposeTensor = (fmapTensor $ fromV3Needle) . transposeTensor . asV3Tensor
  fmapTensor = bilinearFunction $ \f (Tensor (RGB r g b))
                   -> Tensor $ RGB (f $ r) (f $ g) (f $ b)
  fzipTensorWith = bilinearFunction $ \f (Tensor (RGB r g b), Tensor (RGB r' g' b'))
                   -> Tensor $ RGB (f $ (r,r')) (f $ (g,g')) (f $ (b,b'))
  coerceFmapTensorProduct _ Coercion = Coercion

instance LinearSpace ColourNeedle where
  type DualVector ColourNeedle = ColourNeedle
  linearId = LinearMap $ RGB (ColourNeedle $ RGB 1 0 0)
                             (ColourNeedle $ RGB 0 1 0)
                             (ColourNeedle $ RGB 0 0 1)
  coerceDoubleDual = Coercion
  blockVectSpan = LinearFunction $ \w -> Tensor $ RGB (LinearMap $ RGB w o o)
                                                      (LinearMap $ RGB o w o)
                                                      (LinearMap $ RGB o o w)
   where o = zeroV
  contractTensorMap = LinearFunction $ \(LinearMap (RGB (Tensor (RGB r _ _))
                                                        (Tensor (RGB _ g _))
                                                        (Tensor (RGB _ _ b))))
                        -> r ^+^ g ^+^ b
  contractMapTensor = LinearFunction $ \(Tensor (RGB (LinearMap (RGB r _ _))
                                                     (LinearMap (RGB _ g _))
                                                     (LinearMap (RGB _ _ b))))
                        -> r ^+^ g ^+^ b
  contractLinearMapAgainst = bilinearFunction $ \(LinearMap (RGB r g b)) f
                        -> channelRed (getRGBNeedle $ f $ r)
                         + channelGreen (getRGBNeedle $ f $ g)
                         + channelBlue (getRGBNeedle $ f $ b)
  applyDualVector = bilinearFunction $
         \(ColourNeedle (RGB r' g' b')) (ColourNeedle (RGB r g b))
            -> r'*r + g'*g + b'*b
  applyLinear = bilinearFunction $ \(LinearMap (RGB r' g' b')) (ColourNeedle (RGB r g b))
            -> r'^*r ^+^ g'^*g ^+^ b'^*b
  composeLinear = bilinearFunction $ \f (LinearMap (RGB r' g' b'))
            -> LinearMap $ RGB (f $ r') (f $ g') (f $ b')



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
  geodesicBetween a b = return $ \(D¹ q) -> blend ((q+1)/2) b a


class QuantisedColour c where
  quantiseColour :: Colour ℝ -> c

instance QuantisedColour PixelRGBF where
  quantiseColour c = PixelRGBF r g b
   where RGB r g b = fmap realToFrac $ toSRGB c
  
instance QuantisedColour PixelRGB8 where
  quantiseColour c = PixelRGB8 r g b
   where RGB r g b = toSRGB24 c

