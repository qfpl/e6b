{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.PressureAltitude(
  PressureAltitude(..)
, HasPressureAltitude(..)
, convertPressureAltitude
, pressureAltitudeValue
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, HasDistanceUnit(distanceUnit), factorDistanceUnit)
import Papa

data PressureAltitude a =
  PressureAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''PressureAltitude

instance HasDistanceUnit (PressureAltitude a) where
  distanceUnit =
    lens
      (\(PressureAltitude _ u) -> u)
      (\(PressureAltitude a _) u -> PressureAltitude a u)

pressureAltitudeValue ::
  Lens
    (PressureAltitude a)
    (PressureAltitude b)
    a
    b
pressureAltitudeValue =
  lens
    (\(PressureAltitude a _) -> a)
    (\(PressureAltitude _ u) a -> PressureAltitude a u)

convertPressureAltitude ::
  Fractional a =>
  Lens'
    (PressureAltitude a)
    DistanceUnit
convertPressureAltitude =
  lens
    (\(PressureAltitude _ u) -> u)
    (\(PressureAltitude a v) w -> PressureAltitude (factorDistanceUnit v w a) w)
