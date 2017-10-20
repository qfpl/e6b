{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAirspeedPressureAltitudeTemperature(
  IndicatedAirspeedPressureAltitudeTemperature(..)
, HasIndicatedAirspeedPressureAltitudeTemperature(..)
) where

import Data.Aviation.E6B.IndicatedAirspeed
import Data.Aviation.E6B.PressureAltitude
import Data.Aviation.E6B.Temperature
import Papa

data IndicatedAirspeedPressureAltitudeTemperature ias pa temp =
  IndicatedAirspeedPressureAltitudeTemperature
    (IndicatedAirspeed ias)
    (PressureAltitude pa)
    (Temperature temp)
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAirspeedPressureAltitudeTemperature

instance HasIndicatedAirspeed (IndicatedAirspeedPressureAltitudeTemperature ias pa temp) ias where
  indicatedAirspeed =
    lens
      (\(IndicatedAirspeedPressureAltitudeTemperature a _ _) -> a)
      (\(IndicatedAirspeedPressureAltitudeTemperature _ q t) a -> IndicatedAirspeedPressureAltitudeTemperature a q t)

instance HasPressureAltitude (IndicatedAirspeedPressureAltitudeTemperature ias pa temp) pa where
  pressureAltitude =
    lens
      (\(IndicatedAirspeedPressureAltitudeTemperature _ q _) -> q)
      (\(IndicatedAirspeedPressureAltitudeTemperature a _ t) q -> IndicatedAirspeedPressureAltitudeTemperature a q t)

instance HasTemperature (IndicatedAirspeedPressureAltitudeTemperature ias pa temp) temp where
  temperature =
    lens
      (\(IndicatedAirspeedPressureAltitudeTemperature _ _ t) -> t)
      (\(IndicatedAirspeedPressureAltitudeTemperature a q _) t -> IndicatedAirspeedPressureAltitudeTemperature a q t)
