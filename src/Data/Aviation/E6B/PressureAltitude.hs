{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.PressureAltitude(
  PressureUnit(..)
, PressureAltitude(..)
, HasPressureAltitude(..)
, lensPressureAltitude
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit(Pascal, InHg, Psi, Torr, Atmosphere, Bar), factorPressureUnit)
import Papa

data PressureAltitude a =
  PressureAltitude
    a
    PressureUnit
  deriving (Eq, Ord, Show)

makeClassy ''PressureAltitude

lensPressureAltitude ::
  Fractional a =>
  Lens'
    (PressureAltitude a)
    PressureUnit
lensPressureAltitude =
  lens
    (\(PressureAltitude _ u) -> u)
    (\(PressureAltitude a v) w -> PressureAltitude (factorPressureUnit v w a) w)
