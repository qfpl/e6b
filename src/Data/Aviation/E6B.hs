{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-

TODO 20171011

- rename lens* functions on each structure
- write function that sets then gets on that lens
- write lens for each constructor field e.g. PressureAltitude
- fix calculate functions

-}
module Data.Aviation.E6B(
  module E
) where

import Data.Aviation.E6B.DensityAltitude as E
import Data.Aviation.E6B.DistanceUnit as E
import Data.Aviation.E6B.IndicatedAirspeed as E
import Data.Aviation.E6B.PressureAltitude as E
import Data.Aviation.E6B.PressureAltitude as E
import Data.Aviation.E6B.PressureUnit as E
import Data.Aviation.E6B.QNH as E
import Data.Aviation.E6B.Temperature as E
import Data.Aviation.E6B.TemperatureUnit as E
import Data.Aviation.E6B.TrueAirspeed as E
import Data.Aviation.E6B.VelocityUnit as E


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

data DensityAltitudePressureAltitude da pa =
  DensityAltitudePressureAltitude
    (DensityAltitude da)
    (PressureAltitude pa)
  deriving (Eq, Ord, Show)

makeClassy ''DensityAltitudePressureAltitude

instance HasDensityAltitude (DensityAltitudePressureAltitude da pa) da where
  densityAltitude =
    lens
      (\(DensityAltitudePressureAltitude d _) -> d)
      (\(DensityAltitudePressureAltitude _ p) d -> DensityAltitudePressureAltitude d p)

instance HasPressureAltitude (DensityAltitudePressureAltitude da pa) pa where
  pressureAltitude =
    lens
      (\(DensityAltitudePressureAltitude _ p) -> p)
      (\(DensityAltitudePressureAltitude d _) p -> DensityAltitudePressureAltitude d p)

----

test1 ::
  IndicatedAirspeedPressureAltitudeTemperature Double Double Double
test1 =
  IndicatedAirspeedPressureAltitudeTemperature
    (IndicatedAirspeed 105 Knot)
    (PressureAltitude 1000 Metre)
    (Temperature 25 Celsius)

test2 ::
  IndicatedAirspeedPressureAltitudeTemperature Double Double Double
test2 =
  IndicatedAirspeedPressureAltitudeTemperature
    (IndicatedAirspeed 120 Knot)
    (PressureAltitude 8000 Foot)
    (Temperature 288.15 Kelvin)

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

{-
calculateDensityAltitudePressureAltitude ::
  -- (HasAltitude s, HasQNH s, HasTemperature s) =>
  Double
  -> DensityAltitudePressureAltitude
calculateDensityAltitudePressureAltitude =
  calculateDensityAltitudePressureAltitude' altitude qnh temperature

calculateDensityAltitudePressureAltitude' ::
  (
    Unwrapped alt ~ Double
  , Unwrapped qnh ~ Double
  , Unwrapped temp ~ Double
  , Rewrapped alt alt
  , Rewrapped qnh qnh
  , Rewrapped temp temp) =>
  Getting alt Double alt
  -> Getting qnh Double qnh
  -> Getting temp Double temp
  -> Double
  -> DensityAltitudePressureAltitude
calculateDensityAltitudePressureAltitude' getAltitude getQNH getTemperature alt_qnh_temp =
  let alt = alt_qnh_temp ^. getAltitude
      q = alt_qnh_temp ^. getQNH
      tmp = alt_qnh_temp ^. getTemperature
      ea = ((q ^. _Wrapped) ** 0.1903) - (1.313e-5 * (alt ^. _Wrapped))
      pr = ea ** 5.255
      isatemp = (pr / 29.92) / (tmp ^. _Wrapped / 288.15)
      da = 145442.156 * (1 - (isatemp ** 0.234969))
      pa = ((29.92 ** 0.1903) - ea) / 1.313e-5
  in  DensityAltitudePressureAltitude (_Wrapped # da) (_Wrapped # pa)
-}