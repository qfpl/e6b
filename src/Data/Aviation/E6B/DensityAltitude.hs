{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.DensityAltitude(
  DensityAltitude(..)
, HasDensityAltitude(..)
, convertDensityAltitude
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, factorDistanceUnit)
import Papa

data DensityAltitude a =
  DensityAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''DensityAltitude

convertDensityAltitude ::
  Fractional a =>
  Lens'
    (DensityAltitude a)
    DistanceUnit
convertDensityAltitude =
  lens
    (\(DensityAltitude _ u) -> u)
    (\(DensityAltitude a v) w -> DensityAltitude (factorDistanceUnit v w a) w)
