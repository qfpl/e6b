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
        0.539960631749
      factor StatuteMileHour =
        0.868976
      factor MetreSecond =
        1.943843951320815
      factor FootSecond =
        0.59248363636258449816
      f' =
        factor f
      t' =
        factor t
  in  a * f' / t'
