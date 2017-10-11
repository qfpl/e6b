{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.DensityAltitude(
  DensityAltitude(..)
, HasDensityAltitude(..)
, lensDensityAltitude
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, factorDistanceUnit)
import Papa

data DensityAltitude a =
  DensityAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''DensityAltitude

lensDensityAltitude ::
  Fractional a =>
  Lens'
    (DensityAltitude a)
    DistanceUnit
lensDensityAltitude =
  lens
    (\(DensityAltitude _ u) -> u)
    (\(DensityAltitude a v) w -> DensityAltitude (factorDistanceUnit v w a) w)
