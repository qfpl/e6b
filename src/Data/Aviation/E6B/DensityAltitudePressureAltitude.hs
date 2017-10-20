{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.DensityAltitudePressureAltitude(
  DensityAltitudePressureAltitude(..)
, HasDensityAltitudePressureAltitude(..)
, calculateDensityAltitudePressureAltitude
, calculateDensityAltitudePressureAltitude'
) where

import Data.Aviation.E6B.DensityAltitude(DensityAltitude(DensityAltitude), HasDensityAltitude(densityAltitude))
import Data.Aviation.E6B.DistanceUnit(DistanceUnit(Foot))
import Data.Aviation.E6B.IndicatedAltitude(IndicatedAltitude, HasIndicatedAltitude(indicatedAltitude), indicatedAltitudeValue, convertIndicatedAltitude)
import Data.Aviation.E6B.PressureAltitude(PressureAltitude(PressureAltitude), HasPressureAltitude(pressureAltitude))
import Data.Aviation.E6B.PressureUnit(PressureUnit(InHg))
import Data.Aviation.E6B.QNH(QNH, HasQNH(qNH), qnhValue, convertQNH)
import Data.Aviation.E6B.Temperature(Temperature, HasTemperature(temperature), temperatureValue, convertTemperature)
import Data.Aviation.E6B.TemperatureUnit(TemperatureUnit(Kelvin))
import Papa

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

calculateDensityAltitudePressureAltitude ::
  (
    Floating a
  , HasIndicatedAltitude s a
  , HasQNH s a
  , HasTemperature s a
  ) =>
  s
  -> DensityAltitudePressureAltitude a a
calculateDensityAltitudePressureAltitude x =
  calculateDensityAltitudePressureAltitude' indicatedAltitude qNH temperature x

calculateDensityAltitudePressureAltitude' ::
  Floating a =>
  Getting (IndicatedAltitude a) s (IndicatedAltitude a)
  -> Getting (QNH a) s (QNH a)
  -> Getting (Temperature a) s (Temperature a)
  -> s
  -> DensityAltitudePressureAltitude a a
calculateDensityAltitudePressureAltitude' getIndicatedAltitude getQNHValue getTemperatureValue alt_qnh_temp =
  let alt = view indicatedAltitudeValue . set convertIndicatedAltitude Foot . view getIndicatedAltitude $ alt_qnh_temp
      q = view qnhValue . set convertQNH InHg . view getQNHValue $ alt_qnh_temp
      tmp = view temperatureValue . set convertTemperature Kelvin . view getTemperatureValue $ alt_qnh_temp
      ea = (q ** 0.1903) - (1.313e-5 * alt)
      pr = ea ** 5.255
      isatemp = (pr / 29.92) / (tmp / 288.15)
      da = 145442.156 * (1 - (isatemp ** 0.234969))
      pa = ((29.92 ** 0.1903) - ea) / 1.313e-5
  in  DensityAltitudePressureAltitude (DensityAltitude da Foot) (PressureAltitude pa Foot)
