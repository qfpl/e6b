{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.DensityAltitude(
  PressureUnit(..)
, DensityAltitude(..)
, HasDensityAltitude(..)
, lensDensityAltitude
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit(Pascal, InHg, Psi, Torr, Atmosphere, Bar), factorPressureUnit)
import Papa

data DensityAltitude a =
  DensityAltitude
    a
    PressureUnit
  deriving (Eq, Ord, Show)

makeClassy ''DensityAltitude

lensDensityAltitude ::
  Fractional a =>
  Lens'
    (DensityAltitude a)
    PressureUnit
lensDensityAltitude =
  lens
    (\(DensityAltitude _ u) -> u)
    (\(DensityAltitude a v) w -> DensityAltitude (factorPressureUnit v w a) w)
