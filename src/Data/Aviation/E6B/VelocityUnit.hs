{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.E6B.VelocityUnit(
  VelocityUnit(..)
, HasVelocityUnit(..)
, AsVelocityUnit(..)
, factorVelocityUnit
) where

import Papa

data VelocityUnit =
  Knot
  | KilometreHour
  | StatuteMileHour
  | MetreSecond
  | FootSecond
  deriving (Eq, Ord, Show)

makeClassy ''VelocityUnit
makeClassyPrisms ''VelocityUnit

factorVelocityUnit ::
  Fractional a =>
  VelocityUnit
  -> VelocityUnit
  -> a
  -> a
factorVelocityUnit f t a =
  let factor ::
        Fractional a =>
        VelocityUnit
        -> a
      factor Knot =
        1
      factor KilometreHour =
        1.852
      factor StatuteMileHour =
        1.15078
      factor MetreSecond =
        0.514444
      factor FootSecond =
        1.687808398944992
      f' =
        factor f
      t' =
        factor t
  in  a * f' / t'
