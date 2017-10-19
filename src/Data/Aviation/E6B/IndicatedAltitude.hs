{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAltitude(
  IndicatedAltitude(..)
, HasIndicatedAltitude(..)
, convertIndicatedAltitude
, indicatedAltitudeValue
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, HasDistanceUnit(distanceUnit), factorDistanceUnit)
import Papa

data IndicatedAltitude a =
  IndicatedAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAltitude

instance HasDistanceUnit (IndicatedAltitude a) where
  distanceUnit =
    lens
      (\(IndicatedAltitude _ u) -> u)
      (\(IndicatedAltitude a _) u -> IndicatedAltitude a u)

indicatedAltitudeValue ::
  Lens
    (IndicatedAltitude a)
    (IndicatedAltitude b)
    a
    b
indicatedAltitudeValue =
  lens
    (\(IndicatedAltitude a _) -> a)
    (\(IndicatedAltitude _ u) a -> IndicatedAltitude a u)

convertIndicatedAltitude ::
  Fractional a =>
  Lens'
    (IndicatedAltitude a)
    DistanceUnit
convertIndicatedAltitude =
  lens
    (\(IndicatedAltitude _ u) -> u)
    (\(IndicatedAltitude a v) w -> IndicatedAltitude (factorDistanceUnit v w a) w)
