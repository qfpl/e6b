{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.IndicatedAltitude(
  IndicatedAltitude(..)
, HasIndicatedAltitude(..)
, convertIndicatedAltitude
) where

import Data.Aviation.E6B.DistanceUnit(DistanceUnit, factorDistanceUnit)
import Papa

data IndicatedAltitude a =
  IndicatedAltitude
    a
    DistanceUnit
  deriving (Eq, Ord, Show)

makeClassy ''IndicatedAltitude

convertIndicatedAltitude ::
  Fractional a =>
  Lens'
    (IndicatedAltitude a)
    DistanceUnit
convertIndicatedAltitude =
  lens
    (\(IndicatedAltitude _ u) -> u)
    (\(IndicatedAltitude a v) w -> IndicatedAltitude (factorDistanceUnit v w a) w)
