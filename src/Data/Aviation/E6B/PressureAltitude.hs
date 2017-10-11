{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.PressureAltitude(
  PressureAltitude(..)
, HasPressureAltitude(..)
, convertPressureAltitude
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, factorDistanceUnit)
import Papa

data PressureAltitude a =
  PressureAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''PressureAltitude

convertPressureAltitude ::
  Fractional a =>
  Lens'
    (PressureAltitude a)
    DistanceUnit
convertPressureAltitude =
  lens
    (\(PressureAltitude _ u) -> u)
    (\(PressureAltitude a v) w -> PressureAltitude (factorDistanceUnit v w a) w)
