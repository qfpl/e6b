{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.E6B.TemperatureUnit(
  TemperatureUnit(..)
, HasTemperatureUnit(..)
, AsTemperatureUnit(..)
, factorTemperatureUnit
) where

import Papa

data TemperatureUnit =
  Celsius
  | Fahrenheit
  | Kelvin
  deriving (Eq, Ord, Show)

makeClassy ''TemperatureUnit
makeClassyPrisms ''TemperatureUnit

factorTemperatureUnit ::
  Fractional a =>
  TemperatureUnit
  -> TemperatureUnit
  -> a
  -> a
factorTemperatureUnit Celsius Celsius a =
  a
factorTemperatureUnit Celsius Fahrenheit a =
  (9/5) * (a + 32)
factorTemperatureUnit Celsius Kelvin a =
  a + 273.15
factorTemperatureUnit Fahrenheit Fahrenheit a =
  a
factorTemperatureUnit Fahrenheit Celsius a =
  (5/9) * (a - 32)
factorTemperatureUnit Fahrenheit Kelvin a =
  (5/9) * (a + 459.67)
factorTemperatureUnit Kelvin Celsius a =
  a - 273.15
factorTemperatureUnit Kelvin Fahrenheit a =
  (9/5) * (a - 459.67)
factorTemperatureUnit Kelvin Kelvin a =
  a
