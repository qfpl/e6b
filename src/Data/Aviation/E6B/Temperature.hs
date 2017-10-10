{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.Temperature(
  Temperature(..)
, HasTemperature(..)
, temperatureValue
, convertTemperature
) where

import Data.Aviation.E6B.TemperatureUnit(TemperatureUnit, HasTemperatureUnit(temperatureUnit), factorTemperatureUnit)
import Papa

data Temperature a =
  Temperature
    a
    TemperatureUnit
  deriving (Eq, Ord, Show)

makeClassy ''Temperature

instance HasTemperatureUnit (Temperature a) where
  temperatureUnit =
    lens
      (\(Temperature _ u) -> u)
      (\(Temperature a _) u -> Temperature a u)

temperatureValue ::
  Lens
    (Temperature a)
    (Temperature b)
    a
    b
temperatureValue =
  lens
    (\(Temperature a _) -> a)
    (\(Temperature _ u) a -> Temperature a u)

convertTemperature ::
  Fractional a =>
  Lens'
    (Temperature a)
    TemperatureUnit
convertTemperature =
  lens
    (\(Temperature _ u) -> u)
    (\(Temperature a v) w -> Temperature (factorTemperatureUnit v w a) w)
