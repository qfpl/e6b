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

calculateTrueAirspeedDensityAltitude :: (Unwrapped (IndicatedAirspeed a)
      ~
      Unwrapped (DensityAltitude da),
      Unwrapped (PressureAltitude a2) ~ Unwrapped (DensityAltitude da),
      Unwrapped (Temperature a1) ~ Unwrapped (DensityAltitude da),
      Unwrapped (TrueAirspeed tas) ~ Unwrapped (DensityAltitude da),
      HasIndicatedAirspeed s2 a, HasTemperature s2 a1,
      HasPressureAltitude s2 a2,
      Rewrapped (IndicatedAirspeed a) (IndicatedAirspeed a),
      Rewrapped (PressureAltitude a2) (PressureAltitude a2),
      Rewrapped (Temperature a1) (Temperature a1),
      Rewrapped (TrueAirspeed tas) (TrueAirspeed tas),
      Rewrapped (DensityAltitude da) (DensityAltitude da),
      Floating (Unwrapped (DensityAltitude da))) =>
     s2 -> TrueAirspeedDensityAltitude tas da
calculateTrueAirspeedDensityAltitude =
  calculateTrueAirspeedDensityAltitude' pressureAltitude temperature indicatedAirspeed


calculateTrueAirspeedDensityAltitude' :: (Unwrapped
                                            (TrueAirspeed tas)
                                          ~
                                          Unwrapped (DensityAltitude da),
                                          Unwrapped s1 ~ Unwrapped (TrueAirspeed tas),
                                          Unwrapped s3 ~ Unwrapped s1,
                                          Unwrapped s ~ Unwrapped s3,
                                          Floating (Unwrapped (DensityAltitude da)),
                                          Rewrapped (DensityAltitude da) (DensityAltitude da),
                                          Rewrapped (TrueAirspeed tas) (TrueAirspeed tas),
                                          Rewrapped s1 s1, Rewrapped s3 s3, Rewrapped s s) =>
                                         Getting s3 s2 s3
                                         -> Getting s1 s2 s1
                                         -> Getting s s2 s
                                         -> s2
                                         -> TrueAirspeedDensityAltitude tas da

calculateTrueAirspeedDensityAltitude' getPressureAltitude getTemperature getIndicatedAirspeed pa_oat_ias =
  let palt = pa_oat_ias ^. getPressureAltitude -- feet
      tmp = pa_oat_ias ^. getTemperature -- kelvin
      ias = pa_oat_ias ^. getIndicatedAirspeed -- knots
      speed_of_sound = 661.4788 -- knots

      pa_sealevel = 29.92 ** 0.1903 -- Pressure Altitude with constant sea level
      p = (pa_sealevel - 1.313e-5 * palt ^. _Wrapped) ** 5.255
      dalt = (1 - ((p / 29.92) / (tmp ^. _Wrapped / 288.15)) ** 0.234969) * 145442.156
      soundspeed = ias ^. _Wrapped / speed_of_sound
      css = (soundspeed * soundspeed) / 5 + 1
      qc = (css ** 3.5 - 1) * 29.92126 -- impact pressure on pitot tube
      mach = sqrt (5 * ((qc / p + 1) ** (2/7) - 1))
      es = speed_of_sound * mach -- effective airspeed
      ts = es * sqrt ((tmp ^. _Wrapped) / 288.15)
  in  TrueAirspeedDensityAltitude (_Wrapped # ts) (_Wrapped # dalt)

