{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Aviation.E6B where

import Data.Aviation.E6B.DensityAltitude
import Data.Aviation.E6B.IndicatedAltitude
import Data.Aviation.E6B.PressureAltitude
import Data.Aviation.E6B.QNH
import Data.Aviation.E6B.Temperature
import Data.Aviation.E6B.TrueAirspeed
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

data IndicatedAltitudeQNHTemperature ias qnh temp =
  IndicatedAltitudeQNHTemperature
    (IndicatedAltitude ias)
    (QNH qnh)
    (Temperature temp)
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAltitudeQNHTemperature

instance HasIndicatedAltitude (IndicatedAltitudeQNHTemperature ias qnh temp) ias where
  indicatedAltitude =
    lens
      (\(IndicatedAltitudeQNHTemperature a _ _) -> a)
      (\(IndicatedAltitudeQNHTemperature _ q t) a -> IndicatedAltitudeQNHTemperature a q t)

instance HasQNH (IndicatedAltitudeQNHTemperature ias qnh temp) qnh where
  qNH =
    lens
      (\(IndicatedAltitudeQNHTemperature _ q _) -> q)
      (\(IndicatedAltitudeQNHTemperature a _ t) q -> IndicatedAltitudeQNHTemperature a q t)

instance HasTemperature (IndicatedAltitudeQNHTemperature ias qnh temp) temp where
  temperature =
    lens
      (\(IndicatedAltitudeQNHTemperature _ _ t) -> t)
      (\(IndicatedAltitudeQNHTemperature a q _) t -> IndicatedAltitudeQNHTemperature a q t)

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
