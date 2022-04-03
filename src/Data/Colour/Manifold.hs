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

module Data.Colour.Manifold (
         -- * Full colour space
           Colour, QuantisedColour(..)
         -- * 2D/1D projected colour space
         , ColourMap, planarColourMap, colourCurve, colourMapPlane, spectralSwing
         , ColourPlane, cpCold, cpNeutral, cpHot, spanColourPlane 
         -- * Mapping data to colours
         , ColourMappable(..)
         -- * Predefined colour maps
         , SimpleColourMap, blackBlueYellowRed, brightVsRed, redVsBlue
         ) where

import Data.Colour.Manifold.Internal

import Data.Functor (($>))
import Control.Applicative (empty)
import Control.Applicative.Constrained
import Control.Arrow.Constrained
import Data.Semigroup

import Data.Manifold.PseudoAffine
import Math.Manifold.Core.PseudoAffine (GenericNeedle(..))
import Data.Manifold.Types
import Data.Manifold.Atlas
import Data.Manifold.Riemannian
import Data.VectorSpace
import Data.Basis
import Data.AffineSpace
import Data.AdditiveGroup
import Data.Manifold.Shade (Shade(..), Shade'(..)
                           , rangeWithinVertices
                           )
#if MIN_VERSION_manifolds(0,6,0)
import Data.Manifold.WithBoundary
#endif
import Data.Colour.SRGB (toSRGB, toSRGB24)
import Data.Colour.SRGB.Linear
import Data.Colour.RGBSpace.HSL (hslView, hsl)
import Data.Colour hiding (AffineSpace)
import Data.Colour.Names

import Math.LinearMap.Category
import Linear.V2
import Linear.V3

import qualified Prelude as Hask
import Control.Category.Constrained.Prelude

import Codec.Picture.Types

import qualified Test.QuickCheck as QC

import Data.Coerce
import Data.Type.Coercion
import Data.CallStack

import Control.Lens
import GHC.Generics

instance QC.Arbitrary ColourNeedle where
  arbitrary = ColourNeedle <$> (
                   RGB <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary )


asV3Tensor :: (ColourNeedle⊗w) -+> (V3 ℝ⊗w)
asV3Tensor = LinearFunction $ \(Tensor (RGB r g b)) -> Tensor $ V3 r g b

fromV3Tensor :: (V3 ℝ⊗w) -+> (ColourNeedle⊗w)
fromV3Tensor = LinearFunction $ \(Tensor (V3 r g b)) -> Tensor $ RGB r g b

fromV3LinMap :: (V3 ℝ+>w) -+> (ColourNeedle+>w)
fromV3LinMap = LinearFunction $ \(LinearMap (V3 r g b)) -> LinearMap $ RGB r g b

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
  scalarSpaceWitness = ScalarSpaceWitness
  linearManifoldWitness = LinearManifoldWitness
#if !MIN_VERSION_manifolds(0,6,0)
        BoundarylessWitness
#endif
  zeroTensor = Tensor (RGB zeroV zeroV zeroV)
  toFlatTensor = LinearFunction $ \(ColourNeedle (RGB r g b)) -> Tensor (RGB r g b)
  fromFlatTensor = LinearFunction $ \(Tensor (RGB r g b)) -> ColourNeedle (RGB r g b)
  addTensors (Tensor (RGB r g b)) (Tensor (RGB r' g' b'))
                = Tensor $ RGB (r^+^r') (g^+^g') (b^+^b')
  subtractTensors (Tensor (RGB r g b)) (Tensor (RGB r' g' b'))
                = Tensor $ RGB (r^-^r') (g^-^g') (b^-^b')
  negateTensor = LinearFunction $ \(Tensor (RGB r g b))
                       -> Tensor (RGB (negateV r) (negateV g) (negateV b))
  scaleTensor = bilinearFunction $ \μ (Tensor (RGB r g b))
                       -> Tensor (RGB (μ*^r) (μ*^g) (μ*^b))
  tensorProduct = bilinearFunction $ \(ColourNeedle (RGB r g b)) w
                       -> Tensor (RGB (r*^w) (g*^w) (b*^w))
  transposeTensor = (getLinearFunction fmapTensor fromV3Needle)
                      . transposeTensor . asV3Tensor
  fmapTensor = bilinearFunction $ \f (Tensor (RGB r g b))
                   -> Tensor $ RGB (f $ r) (f $ g) (f $ b)
  fzipTensorWith = bilinearFunction $ \f (Tensor (RGB r g b), Tensor (RGB r' g' b'))
                   -> Tensor $ RGB (f $ (r,r')) (f $ (g,g')) (f $ (b,b'))
  coerceFmapTensorProduct _ Coercion = Coercion
  wellDefinedTensor t@(Tensor (RGB r g b))
    = wellDefinedVector r >> wellDefinedVector g >> wellDefinedVector b $> t

instance LinearSpace ColourNeedle where
  type DualVector ColourNeedle = ColourNeedle
  linearId = LinearMap $ RGB (ColourNeedle $ RGB 1 0 0)
                             (ColourNeedle $ RGB 0 1 0)
                             (ColourNeedle $ RGB 0 0 1)
  tensorId = ti dualSpaceWitness (asTensor $ id)
   where ti :: ∀ w . (TensorSpace w, Scalar w ~ ℝ)
               => DualSpaceWitness w -> Tensor ℝ (DualVector w) w
                 -> Tensor ℝ ColourNeedle w+>Tensor ℝ ColourNeedle w
         ti DualSpaceWitness wid = LinearMap $ RGB
                  (fmap (LinearFunction $ \w -> Tensor $ RGB w zeroV zeroV) $ wid)
                  (fmap (LinearFunction $ \w -> Tensor $ RGB zeroV w zeroV) $ wid)
                  (fmap (LinearFunction $ \w -> Tensor $ RGB zeroV zeroV w) $ wid)
  coerceDoubleDual = Coercion
  dualSpaceWitness = DualSpaceWitness
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
  applyTensorFunctional = bilinearFunction
            $ \(LinearMap (RGB r' g' b')) (Tensor (RGB r g b))
                   -> r'<.>^r + g'<.>^g + b'<.>^b
  applyTensorLinMap = bilinearFunction
            $ \(LinearMap (RGB r' g' b')) (Tensor (RGB r g b))
                -> (r'+$r) ^+^ (g'+$g) ^+^ (b'+$b)
   where f+$x = getLinearFunction (getLinearFunction applyLinear $ fromTensor $ f) x
  composeLinear = bilinearFunction $ \f (LinearMap (RGB r' g' b'))
            -> LinearMap $ RGB (f +$ r') (f +$ g') (f +$ b')
   where f+$x = getLinearFunction (getLinearFunction applyLinear f) x
  useTupleLinearSpaceComponents = undefined

instance SemiInner ColourNeedle where
  dualBasisCandidates = cartesianDualBasisCandidates
           [ColourNeedle (RGB 1 0 0), ColourNeedle (RGB 0 1 0), ColourNeedle (RGB 0 0 1)]
           (\(ColourNeedle (RGB r g b)) -> abs <$> [r,g,b])
  tensorDualBasisCandidates = map (second $ getLinearFunction asV3Tensor)
                           >>> tensorDualBasisCandidates
                           >>> map (fmap $ second $ getLinearFunction fromV3LinMap)

instance FiniteDimensional ColourNeedle where
  data SubBasis ColourNeedle = ColourNeedleBasis
  entireBasis = ColourNeedleBasis
  enumerateSubBasis ColourNeedleBasis
          = ColourNeedle <$> [RGB 1 0 0, RGB 0 1 0, RGB 0 0 1]
  decomposeLinMap (LinearMap (RGB r g b)) = (ColourNeedleBasis, ([r,g,b]++))
  decomposeLinMapWithin ColourNeedleBasis (LinearMap (RGB r g b)) = pure ([r,g,b]++)
  recomposeSB ColourNeedleBasis [] = (ColourNeedle $ RGB 0 0 0, [])
  recomposeSB ColourNeedleBasis [r] = (ColourNeedle $ RGB r 0 0, [])
  recomposeSB ColourNeedleBasis [r,g] = (ColourNeedle $ RGB r g 0, [])
  recomposeSB ColourNeedleBasis (r:g:b:l) = (ColourNeedle $ RGB r g b, l)
  recomposeSBTensor ColourNeedleBasis sbw l
          = let (r,l') = recomposeSB sbw l
                (g,l'') = recomposeSB sbw l'
                (b,l''') = recomposeSB sbw l''
            in (Tensor $ RGB r g b, l''')
  recomposeLinMap ColourNeedleBasis [] = (LinearMap $ RGB zeroV zeroV zeroV, [])
  recomposeLinMap ColourNeedleBasis [r] = (LinearMap $ RGB r zeroV zeroV, [])
  recomposeLinMap ColourNeedleBasis [r,g] = (LinearMap $ RGB r g zeroV, [])
  recomposeLinMap ColourNeedleBasis (r:g:b:l) = (LinearMap $ RGB r g b, l)
  recomposeContraLinMap f l = LinearMap $ RGB (f $ fmap (channelRed . getRGBNeedle) l)
                                              (f $ fmap (channelGreen . getRGBNeedle) l)
                                              (f $ fmap (channelBlue . getRGBNeedle) l)
  tensorEquality (Tensor t) (Tensor τ) = t == τ
  recomposeContraLinMapTensor = rclmt dualSpaceWitness
   where rclmt :: ∀ u w f . ( Hask.Functor f
                            , FiniteDimensional u, LinearSpace w
                            , Scalar u ~ ℝ, Scalar w ~ ℝ )
                          => DualSpaceWitness u
                         -> (f ℝ -> w) -> f (ColourNeedle+>DualVector u)
                            -> (ColourNeedle⊗u)+>w
         rclmt DualSpaceWitness fw mv = LinearMap $
           (\c -> fromLinearMap $ recomposeContraLinMap fw
                $ fmap (\(LinearMap q) -> c q) mv)
                       <$> RGB channelRed channelGreen channelBlue
  uncanonicallyFromDual = id
  uncanonicallyToDual = id

fromLinearMap :: ∀ s u v w . (LinearSpace u, Scalar u ~ s)
                 => LinearMap s (DualVector u) w -> Tensor s u w
fromLinearMap = case dualSpaceWitness :: DualSpaceWitness u of
    DualSpaceWitness -> coerce
asTensor :: ∀ s u v w . (LinearSpace u, Scalar u ~ s)
                 => LinearMap s u w -> Tensor s (DualVector u) w
asTensor = coerce
fromTensor :: ∀ s u v w . (LinearSpace u, Scalar u ~ s)
                 => Tensor s (DualVector u) w -> LinearMap s u w
fromTensor = coerce

  

instance Semimanifold ColourNeedle where
  type Needle ColourNeedle = ColourNeedle
#if MIN_VERSION_manifolds(0,6,0)
  (.+~^) = (^+^)
#else
  fromInterior = id; toInterior = pure
  translateP = pure (^+^)
#endif

instance PseudoAffine ColourNeedle where
  ColourNeedle q .-~! ColourNeedle s = ColourNeedle $ liftA2 (-) q s
  q .-~. s = pure (q.-~!s)

instance Atlas ColourNeedle where
  type ChartIndex ColourNeedle = ()
#if !MIN_VERSION_manifolds(0,6,0)
  interiorChartReferencePoint _ () = zeroV
#else
  chartReferencePoint () = zeroV
#endif
  lookupAtlas _ = ()

#if MIN_VERSION_manifolds(0,6,0)
instance SemimanifoldWithBoundary ColourNeedle where
  type Interior ColourNeedle = ColourNeedle
  type Boundary ColourNeedle = EmptyMfd ℝ⁰
  type HalfNeedle ColourNeedle = ℝay
  smfdWBoundWitness = OpenManifoldWitness
  (|+^) b = case b of {}
  _ .+^| b = case b of {}
  fromBoundary b = case b of {}
  fromInterior = id

instance PseudoAffineWithBoundary ColourNeedle where
  _ !-| b = case b of {}
  (.--!) = (.-~!)

instance ProjectableBoundary ColourNeedle where
  projectToBoundary _ b = case b of {}
  marginFromBoundary b _ = case b of {}
#endif

instance AffineSpace ColourNeedle where
  type Diff ColourNeedle = ColourNeedle
  (.-.) = (.-~!)
  (.+^) = (.+~^)

fromLtdRGB :: LtdCol -> Colour ℝ
fromLtdRGB = fmap (\(CD¹ h Origin) -> h) >>> \(RGB r g b) -> rgb r g b

toLtdRGB :: Colour ℝ -> LtdCol
toLtdRGB = toRGB >>> fmap ((`CD¹`Origin) . min 1 . max 0)

type LtdCol = RGB (CD¹ ℝ⁰)

bijectToLtd :: ℝ -> CD¹ ℝ⁰
bijectToLtd 0 = CD¹ 0.5 Origin
bijectToLtd y
  | ψ > 0.5    = CD¹ 1 Origin
  | ψ > -0.5   = CD¹ ( 0.5 - ψ ) Origin
  | otherwise  = CD¹ 0 Origin
 where ψ = (1 - sqrt(1+y^2)) / (2*y)
-- y = (x - 1/2) / (x*(1 - x))
-- y * x * (1 - x) = x - 1/2
-- y * x² - (1 - y) * x - 1/2 = 0
-- y * x² + (y - 1) * x - 1/2 = 0
-- x = (1 - y ± sqrt( (1-y)² + 2*y ) ) / (-2*y)
--   = (y - 1 +! sqrt( 1 + y² ) ) / (2*y)  -- unstable for y ≈ 0
--   = 1/2 - (1 - sqrt( 1 + y² ) ) / (2*y)

bijectFromLtd :: CD¹ ℝ⁰ -> Either S⁰ ℝ
bijectFromLtd (CD¹ x Origin)
    | x<=1e-9     = Left NegativeHalfSphere
    | x>=1-1e-9   = Left PositiveHalfSphere
    | otherwise   = return $ (x - 0.5) / (x*(1 - x))



#if MIN_VERSION_manifolds(0,6,0)
instance AdditiveMonoid ColourHalfNeedle
instance HalfSpace ColourHalfNeedle
#endif

#if MIN_VERSION_manifolds(0,6,0)
instance QC.Arbitrary ColourBoundary where
  arbitrary = ColourBoundarySphere <$> QC.arbitrary

instance SemimanifoldWithBoundary ColourBoundary where
  type Boundary ColourBoundary = EmptyMfd ℝ⁰
  type Interior ColourBoundary = ColourBoundary
  type HalfNeedle ColourBoundary = ℝay
  smfdWBoundWitness = OpenManifoldWitness
  needleIsOpenMfd q = q
  b|+^_ = case b of {}
  _.+^|b = case b of {}
  fromInterior = id
  fromBoundary b = case b of {}
#endif

instance Hask.Foldable RGB where
  foldMap f (RGB r g b) = f r `mappend` f g `mappend` f b

projectRGBToColourBoundary :: RGB ℝ -> ColourBoundary
projectRGBToColourBoundary c = ColourBoundarySphere $ S²Polar ϑ φ
 where (h,_,l) = hslView c
       φ = h*2*pi/360 - pi
       ϑ = l * pi

#if MIN_VERSION_manifolds(0,6,0)
instance SemimanifoldWithBoundary (Colour ℝ) where
  type Boundary (Colour ℝ) = ColourBoundary
  type HalfNeedle (Colour ℝ) = ColourHalfNeedle
  smfdWBoundWitness = undefined -- SmfdWBoundWitness
  needleIsOpenMfd q = q
  fromBoundary (ColourBoundarySphere (S²Polar ϑ φ))
        = fromRGB $ hsl ((φ+pi)*360/(2*pi)) 1 (ϑ/pi)
  b |+^ ColourHalfNeedle (Cℝay d Origin) δb
        = fromRGB $ hsl ((φ+pi)*360/(2*pi)) (1/(d+1)) (0.5 + (ϑ/pi-0.5)/(d+1))
   where ColourBoundarySphere (S²Polar ϑ φ) = b.+~^δb
  c .+^| ColourNeedle dc
    | η>1        = Left (projectRGBToColourBoundary $ (+).(/η) <$> dc <*> rgb, η - 1)
    | otherwise  = case separateInterior . fromRGB $ (+)<$>dc<*>rgb of
                     Right c' -> Right c'
                     Left c'b -> error $ show (η, (+)<$>dc<*>rgb)
   where rgb = toRGB c
         η = maximum $ (\m d -> if d>0 then if m<1 then d/(1-m) else huge
                                 else if d<0 then -d/m
                                 else 0)
                      <$> rgb <*> dc
         huge = 1e12
  separateInterior c = case toin $ toLtdRGB c of
           Left _ -> Left . projectRGBToColourBoundary $ toRGB c
           Right ci -> Right $ ColourNeedle ci
   where rgb = toRGB c
         toin (RGB r g b) = liftA3 RGB (bijectFromLtd r) (bijectFromLtd g) (bijectFromLtd b)
#else
instance Semimanifold (Colour ℝ) where
  type Needle (Colour ℝ) = ColourNeedle
#endif
  type Interior (Colour ℝ) = ColourNeedle
  fromInterior (ColourNeedle q) = fromLtdRGB $ fmap bijectToLtd q
  toInterior = fmap ColourNeedle . eitherToMaybe . toin . toLtdRGB
   where toin (RGB r g b) = liftA3 RGB (bijectFromLtd r) (bijectFromLtd g) (bijectFromLtd b)
#if !MIN_VERSION_manifolds(0,6,0)
  translateP = pure (^+^)
#endif

#if MIN_VERSION_manifolds(0,6,0)
instance PseudoAffineWithBoundary (Colour ℝ) where
  c .--! d = ColourNeedle $ (-) <$> toRGB c <*> toRGB d
#else
instance PseudoAffine (Colour ℝ) where
  c .-~. ζ = liftA2 (^-^) (toInterior c) (toInterior ζ)
#endif

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

instance Geodesic (Colour ℝ) where
  geodesicBetween a b = return $ \(D¹ q) -> blend ((q+1)/2) b a

instance Geodesic ColourNeedle where
  geodesicBetween (ColourNeedle (RGB r g b)) (ColourNeedle (RGB r' g' b'))
                 = return $ \(D¹ q) -> let η' = (q+1)/2 in ColourNeedle
                                        $ RGB (lerp r r' η')
                                              (lerp g g' η')
                                              (lerp b b' η')

instance Atlas (Colour ℝ) where
  type ChartIndex (Colour ℝ) = ()
  chartReferencePoint () = grey
#if !MIN_VERSION_manifolds(0,6,0)
  interiorChartReferencePoint = \_ () -> intGrey
   where Just intGrey = toInterior (grey :: Colour ℝ)
#endif
  lookupAtlas _ = ()

class QuantisedColour c where
  quantiseColour :: Colour ℝ -> c

instance QuantisedColour PixelRGBF where
  quantiseColour c = PixelRGBF r g b
   where RGB r g b = fmap realToFrac $ toSRGB c
  
instance QuantisedColour PixelRGB8 where
  quantiseColour c = PixelRGB8 r g b
   where RGB r g b = toSRGB24 c


-- | A two-dimensional, smoothly varying colour palette.
data ColourMap x = ColourMap {
       _cmPlane :: ColourPlane
     , _cmSpectSwing :: ℝ
     }

planarColourMap :: ColourPlane -> ColourMap x
planarColourMap = (`ColourMap`0)

colourCurve :: ColourPlane -> ℝ -> ColourMap ℝ
colourCurve = ColourMap

spectralSwing :: (Needle x ~ ℝ) => Traversal' (ColourMap x) ℝ
spectralSwing = lens _cmSpectSwing (\cm sw' -> cm{_cmSpectSwing = sw'})

colourMapPlane :: Traversal' (ColourMap x) ColourPlane
colourMapPlane = lens _cmPlane (\cm pl' -> cm{_cmPlane = pl'})

fromRGB :: Fractional a => RGB a -> Colour a
fromRGB (RGB r g b) = rgb r g b

data ColourPlane = ColourPlane {
        _cpCold :: Colour ℝ
      , _cpNeutral :: Interior (Colour ℝ)
      , _cpHot :: Colour ℝ
      }
makeLenses ''ColourPlane

spanColourPlane :: Interior (Colour ℝ)   -- ^ Neutral colour
                -> (Colour ℝ, Colour ℝ)  -- ^ Extreme “cold” / “hot” colours
                -> ColourPlane
spanColourPlane neutral (cold,hot) = ColourPlane cold neutral hot

class Geodesic x => ColourMappable x where
  type ColourMapped x :: *
  type MappingVertex x :: *
  mapToColourWith :: HasCallStack
                  => ColourMap (MappingVertex x)
                  -> Interior (MappingVertex x)
                  -> (MappingVertex x, MappingVertex x)
                  -> x
                  -> ColourMapped x

instance ColourMappable ℝ where
  type ColourMapped ℝ = Colour ℝ
  type MappingVertex ℝ = ℝ
  mapToColourWith (ColourMap (ColourPlane coldC neutralC hotC) swing)
              neutralP (coldP, hotP)
        = (\(Shade c _) -> fromInterior c)
           . shFn
           . \x -> let φ = 2*(x-neutralP)/(hotP-coldP)
                   in Shade ( (1 - φ)/2 + (φ^2 - 1)*exp swing/2
                            , (φ + 1)/2 + (φ^2 - 1)*exp swing/2 )
                            (spanNorm [(256,0), (0,256)])
                                     :: Shade (ℝ,ℝ)
   where Just shFn = rangeWithinVertices ((0,0), neutralC)
                                        [((1,0) :: (ℝ,ℝ), coldC), ((0,1), hotC)]

instance ColourMappable (ℝ,ℝ) where
  type ColourMapped (ℝ,ℝ) = Colour ℝ
  type MappingVertex (ℝ,ℝ) = (ℝ,ℝ)
  mapToColourWith (ColourMap cp swing)
              (xN,yN) ((xCold,yCold), (xHot,yHot))
      = mapToColourWith (ColourMap cp swing) (V2 xN yN) (V2 xCold yCold, V2 xHot yHot)
          . \(x,y) -> (V2 x y)

instance ColourMappable ℝ² where
  type ColourMapped ℝ² = Colour ℝ
  type MappingVertex ℝ² = ℝ²
  mapToColourWith (ColourMap (ColourPlane coldC neutralC hotC) swing)
              neutralP (coldP, hotP)
        = (\(Shade c _) -> fromInterior c)
           . shFn
           . \xy -> Shade xy quantisationNorm
   where Just shFn = rangeWithinVertices (neutralP, neutralC)
                                        [(coldP, coldC), (hotP, hotC)]
         quantisationNorm = scaleNorm 256 . dualNorm
                              $ spanVariance [coldP^-^neutralP, hotP^-^neutralP]


class ColourMappable x => HasSimpleColourMaps x where
  simpleColourMap :: ColourPlane -> ℝ -> ColourMap x
  simpleColourMap = const . planarColourMap

instance HasSimpleColourMaps ℝ where
  simpleColourMap = colourCurve

instance HasSimpleColourMaps (ℝ,ℝ)
instance HasSimpleColourMaps ℝ²

type SimpleColourMap = ∀ x . HasSimpleColourMaps x => ColourMap x

blackBlueYellowRed :: SimpleColourMap
blackBlueYellowRed
   = simpleColourMap (spanColourPlane neutralc (darkblue,goldenrod)) 1
 where Just neutralc = toInterior (dimgrey :: Colour ℝ)

redVsBlue :: SimpleColourMap
redVsBlue
   = simpleColourMap (spanColourPlane neutralc (rgb 0.9 0 0.2, rgb 0.1 0.3 1)) (-1/2)
 where neutralc = ColourNeedle $ RGB (-1.2) (-0.5) (-1.5)

brightVsRed :: SimpleColourMap
brightVsRed
   = simpleColourMap (spanColourPlane neutralc (white, orangered)) 1
 where Just neutralc = toInterior (darkgrey :: Colour ℝ)
