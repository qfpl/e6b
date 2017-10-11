{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.Temperature(
  Temperature(..)
, HasTemperature(..)
, lensTemperature
) where

import Data.Aviation.E6B.TemperatureUnit(TemperatureUnit, factorTemperatureUnit)
import Papa

data Temperature a =
  Temperature
    a
    TemperatureUnit
  deriving (Eq, Ord, Show)

makeClassy ''Temperature

lensTemperature ::
  Fractional a =>
  Lens'
    (Temperature a)
    TemperatureUnit
lensTemperature =
  lens
    (\(Temperature _ u) -> u)
    (\(Temperature a v) w -> Temperature (factorTemperatureUnit v w a) w)
