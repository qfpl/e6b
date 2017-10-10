{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.DensityAltitude(
  DensityAltitude(..)
, HasDensityAltitude(..)
, densityAltitudeValue
, convertDensityAltitude
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, HasDistanceUnit(distanceUnit), factorDistanceUnit)
import Papa

data DensityAltitude a =
  DensityAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''DensityAltitude

instance HasDistanceUnit (DensityAltitude a) where
  distanceUnit =
    lens
      (\(DensityAltitude _ u) -> u)
      (\(DensityAltitude a _) u -> DensityAltitude a u)

densityAltitudeValue ::
  Lens
    (DensityAltitude a)
    (DensityAltitude b)
    a
    b
densityAltitudeValue =
  lens
    (\(DensityAltitude a _) -> a)
    (\(DensityAltitude _ u) a -> DensityAltitude a u)

convertDensityAltitude ::
  Fractional a =>
  Lens'
    (DensityAltitude a)
    DistanceUnit
convertDensityAltitude =
  lens
    (\(DensityAltitude _ u) -> u)
    (\(DensityAltitude a v) w -> DensityAltitude (factorDistanceUnit v w a) w)
