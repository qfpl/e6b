{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAltitude(
  PressureUnit(..)
, IndicatedAltitude(..)
, HasIndicatedAltitude(..)
, lensIndicatedAltitude
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit(Pascal, InHg, Psi, Torr, Atmosphere, Bar), factorPressureUnit)
import Papa

data IndicatedAltitude a =
  IndicatedAltitude
    a
    PressureUnit
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAltitude

lensIndicatedAltitude ::
  Fractional a =>
  Lens'
    (IndicatedAltitude a)
    PressureUnit
lensIndicatedAltitude =
  lens
    (\(IndicatedAltitude _ u) -> u)
    (\(IndicatedAltitude a v) w -> IndicatedAltitude (factorPressureUnit v w a) w)
