{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.Temperature(
  Temperature(..)
, HasTemperature(..)
, convertTemperature
) where

import Data.Aviation.E6B.TemperatureUnit(TemperatureUnit, factorTemperatureUnit)
import Papa

data Temperature a =
  Temperature
    a
    TemperatureUnit
  deriving (Eq, Ord, Show)

makeClassy ''Temperature

convertTemperature ::
  Fractional a =>
  Lens'
    (Temperature a)
    TemperatureUnit
convertTemperature =
  lens
    (\(Temperature _ u) -> u)
    (\(Temperature a v) w -> Temperature (factorTemperatureUnit v w a) w)
