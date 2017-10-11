{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAirspeed(
  IndicatedAirspeed(..)
, HasIndicatedAirspeed(..)
, lensIndicatedAirspeed
) where

import Data.Aviation.E6B.VelocityUnit(VelocityUnit, factorVelocityUnit)
import Papa

data IndicatedAirspeed a =
  IndicatedAirspeed
    a
    VelocityUnit
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAirspeed

lensIndicatedAirspeed ::
  Fractional a =>
  Lens'
    (IndicatedAirspeed a)
    VelocityUnit
lensIndicatedAirspeed =
  lens
    (\(IndicatedAirspeed _ u) -> u)
    (\(IndicatedAirspeed a v) w -> IndicatedAirspeed (factorVelocityUnit v w a) w)
