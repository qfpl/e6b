{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.TrueAirspeed(
  TrueAirspeed(..)
, HasTrueAirspeed(..)
, convertTrueAirspeed
) where

import Data.Aviation.E6B.VelocityUnit(VelocityUnit, factorVelocityUnit)
import Papa

data TrueAirspeed a =
  TrueAirspeed
    a
    VelocityUnit
  deriving (Eq, Ord, Show)

makeClassy ''TrueAirspeed

convertTrueAirspeed ::
  Fractional a =>
  Lens'
    (TrueAirspeed a)
    VelocityUnit
convertTrueAirspeed =
  lens
    (\(TrueAirspeed _ u) -> u)
    (\(TrueAirspeed a v) w -> TrueAirspeed (factorVelocityUnit v w a) w)
