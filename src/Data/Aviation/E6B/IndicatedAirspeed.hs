{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAirspeed(
  IndicatedAirspeed(..)
, HasIndicatedAirspeed(..)
, convertIndicatedAirspeed
, indicatedAirspeedValue
) where

import Data.Aviation.E6B.VelocityUnit(VelocityUnit, HasVelocityUnit(velocityUnit), factorVelocityUnit)
import Papa

data IndicatedAirspeed a =
  IndicatedAirspeed
    a
    VelocityUnit
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAirspeed

instance HasVelocityUnit (IndicatedAirspeed a) where
  velocityUnit =
    lens
      (\(IndicatedAirspeed _ u) -> u)
      (\(IndicatedAirspeed a _) u -> IndicatedAirspeed a u)

indicatedAirspeedValue ::
  Lens
    (IndicatedAirspeed a)
    (IndicatedAirspeed b)
    a
    b
indicatedAirspeedValue =
  lens
    (\(IndicatedAirspeed a _) -> a)
    (\(IndicatedAirspeed _ u) a -> IndicatedAirspeed a u)

convertIndicatedAirspeed ::
  Fractional a =>
  Lens'
    (IndicatedAirspeed a)
    VelocityUnit
convertIndicatedAirspeed =
  lens
    (\(IndicatedAirspeed _ u) -> u)
    (\(IndicatedAirspeed a v) w -> IndicatedAirspeed (factorVelocityUnit v w a) w)
