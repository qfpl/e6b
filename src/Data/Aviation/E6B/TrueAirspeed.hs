{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.TrueAirspeed(
  TrueAirspeed(..)
, HasTrueAirspeed(..)
, trueAirspeedValue
, convertTrueAirspeed
) where

import Data.Aviation.E6B.VelocityUnit(VelocityUnit, HasVelocityUnit(velocityUnit), factorVelocityUnit)
import Papa

data TrueAirspeed a =
  TrueAirspeed
    a
    VelocityUnit
  deriving (Eq, Ord, Show)

makeClassy ''TrueAirspeed

instance HasVelocityUnit (TrueAirspeed a) where
  velocityUnit =
    lens
      (\(TrueAirspeed _ u) -> u)
      (\(TrueAirspeed a _) u -> TrueAirspeed a u)

trueAirspeedValue ::
  Lens
    (TrueAirspeed a)
    (TrueAirspeed b)
    a
    b
trueAirspeedValue =
  lens
    (\(TrueAirspeed a _) -> a)
    (\(TrueAirspeed _ u) a -> TrueAirspeed a u)

convertTrueAirspeed ::
  Fractional a =>
  Lens'
    (TrueAirspeed a)
    VelocityUnit
convertTrueAirspeed =
  lens
    (\(TrueAirspeed _ u) -> u)
    (\(TrueAirspeed a v) w -> TrueAirspeed (factorVelocityUnit v w a) w)
