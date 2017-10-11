{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.PressureAltitude(
  DistanceUnit(..)
, PressureAltitude(..)
, HasPressureAltitude(..)
, lensPressureAltitude
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit(Kilometre, Metre, Centimetre, Millimetre, Micrometre, Nanometre, StatuteMile, Yard, Foot, Inch, NauticalMile), factorDistanceUnit)
import Papa

data PressureAltitude a =
  PressureAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''PressureAltitude

lensPressureAltitude ::
  Fractional a =>
  Lens'
    (PressureAltitude a)
    DistanceUnit
lensPressureAltitude =
  lens
    (\(PressureAltitude _ u) -> u)
    (\(PressureAltitude a v) w -> PressureAltitude (factorDistanceUnit v w a) w)
