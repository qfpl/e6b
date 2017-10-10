{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAltitudeQNHTemperature(
  IndicatedAltitudeQNHTemperature(..)
, HasIndicatedAltitudeQNHTemperature(..)
) where

import Data.Aviation.E6B.IndicatedAltitude
import Data.Aviation.E6B.QNH
import Data.Aviation.E6B.Temperature
import Papa

data IndicatedAltitudeQNHTemperature ia qnh tmp =
  IndicatedAltitudeQNHTemperature
    (IndicatedAltitude ia)
    (QNH qnh)
    (Temperature tmp)
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAltitudeQNHTemperature

instance HasIndicatedAltitude (IndicatedAltitudeQNHTemperature ia qnh tmp) ia where
  indicatedAltitude =
    lens
      (\(IndicatedAltitudeQNHTemperature i _ _) -> i)
      (\(IndicatedAltitudeQNHTemperature _ q t) i -> IndicatedAltitudeQNHTemperature i q t)

instance HasQNH (IndicatedAltitudeQNHTemperature ia qnh tmp) qnh where
  qNH =
    lens
      (\(IndicatedAltitudeQNHTemperature _ q _) -> q)
      (\(IndicatedAltitudeQNHTemperature i _ t) q -> IndicatedAltitudeQNHTemperature i q t)

instance HasTemperature (IndicatedAltitudeQNHTemperature ia qnh tmp) tmp where
  temperature =
    lens
      (\(IndicatedAltitudeQNHTemperature _ _ t) -> t)
      (\(IndicatedAltitudeQNHTemperature i q _) t -> IndicatedAltitudeQNHTemperature i q t)
