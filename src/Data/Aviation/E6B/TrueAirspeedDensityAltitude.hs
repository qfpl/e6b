{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.TrueAirspeedDensityAltitude(
  TrueAirspeedDensityAltitude(..)
, HasTrueAirspeedDensityAltitude(..)
, calculateTrueAirspeedDensityAltitude
, calculateTrueAirspeedDensityAltitude'
) where

import Data.Aviation.E6B.DensityAltitude(DensityAltitude(DensityAltitude), HasDensityAltitude(densityAltitude))
import Data.Aviation.E6B.DistanceUnit(DistanceUnit(Foot))
import Data.Aviation.E6B.IndicatedAirspeed(IndicatedAirspeed, HasIndicatedAirspeed(indicatedAirspeed), indicatedAirspeedValue, convertIndicatedAirspeed)
import Data.Aviation.E6B.PressureAltitude(PressureAltitude, HasPressureAltitude(pressureAltitude), pressureAltitudeValue, convertPressureAltitude)
import Data.Aviation.E6B.Temperature(Temperature, HasTemperature(temperature), temperatureValue, convertTemperature)
import Data.Aviation.E6B.TemperatureUnit(TemperatureUnit(Kelvin))
import Data.Aviation.E6B.TrueAirspeed(TrueAirspeed(TrueAirspeed), HasTrueAirspeed(trueAirspeed))
import Data.Aviation.E6B.VelocityUnit(VelocityUnit(Knot))
import Papa

data TrueAirspeedDensityAltitude tas da =
  TrueAirspeedDensityAltitude
    (TrueAirspeed tas)
    (DensityAltitude da)
  deriving (Eq, Ord, Show)

makeClassy ''TrueAirspeedDensityAltitude

instance HasTrueAirspeed (TrueAirspeedDensityAltitude tas da) tas where
  trueAirspeed =
    lens
      (\(TrueAirspeedDensityAltitude t _) -> t)
      (\(TrueAirspeedDensityAltitude _ d) t -> TrueAirspeedDensityAltitude t d)

instance HasDensityAltitude (TrueAirspeedDensityAltitude tas da) da where
  densityAltitude =
    lens
      (\(TrueAirspeedDensityAltitude _ d) -> d)
      (\(TrueAirspeedDensityAltitude t _) d -> TrueAirspeedDensityAltitude t d)

calculateTrueAirspeedDensityAltitude ::
  (
    Floating a
  , HasPressureAltitude s a
  , HasTemperature s a
  , HasIndicatedAirspeed s a
  ) =>
  s
  -> TrueAirspeedDensityAltitude a a
calculateTrueAirspeedDensityAltitude x =
  calculateTrueAirspeedDensityAltitude' pressureAltitude temperature indicatedAirspeed x

calculateTrueAirspeedDensityAltitude' ::
  Floating a =>
  Getting (PressureAltitude a) s (PressureAltitude a)
  -> Getting (Temperature a) s (Temperature a)
  -> Getting (IndicatedAirspeed a) s (IndicatedAirspeed a)
  -> s
  -> TrueAirspeedDensityAltitude a a
calculateTrueAirspeedDensityAltitude' getPressureAltitude getTemperature getIndicatedAirspeed pa_oat_ias =
  let palt = view pressureAltitudeValue . set convertPressureAltitude Foot . view getPressureAltitude $ pa_oat_ias
      tmp = view temperatureValue . set convertTemperature Kelvin . view getTemperature $ pa_oat_ias
      ias = view indicatedAirspeedValue . set convertIndicatedAirspeed Knot . view getIndicatedAirspeed $ pa_oat_ias
      speed_of_sound = 661.4788 -- knots
      pa_sealevel = 29.92 ** 0.1903 -- Pressure Altitude with constant sea level
      p = (pa_sealevel - 1.313e-5 * palt) ** 5.255
      dalt = (1 - ((p / 29.92) / (tmp / 288.15)) ** 0.234969) * 145442.156
      soundspeed = ias / speed_of_sound
      css = (soundspeed * soundspeed) / 5 + 1
      qc = (css ** 3.5 - 1) * 29.92126 -- impact pressure on pitot tube
      mach = sqrt (5 * ((qc / p + 1) ** (2/7) - 1))
      es = speed_of_sound * mach -- effective airspeed
      ts = es * sqrt (tmp / 288.15)
  in  TrueAirspeedDensityAltitude (TrueAirspeed ts Knot) (DensityAltitude dalt Foot)
