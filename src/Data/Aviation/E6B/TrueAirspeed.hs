{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.E6B.TrueAirspeed(
  VelocityUnit(..)
, TrueAirspeed(..)
, lensTrueAirspeed
) where

import Data.Aviation.E6B.VelocityUnit(VelocityUnit(Knot, KilometreHour, StatuteMileHour, MetreSecond, FootSecond))
import Papa

data TrueAirspeed a =
  TrueAirspeed
    a
    VelocityUnit
  deriving (Eq, Ord, Show)

lensTrueAirspeed ::
  Fractional a =>
  Lens'
    (TrueAirspeed a)
    VelocityUnit
lensTrueAirspeed =
  lens
    (\(TrueAirspeed _ u) -> u)
    (\(TrueAirspeed a v) w ->
      TrueAirspeed
        (
          let factor ::
                Fractional a =>
                VelocityUnit
                -> a
              factor Knot =
                0.00029529983071445
              factor KilometreHour =
                1
              factor StatuteMileHour =
                2.036025
              factor MetreSecond =
                0.0393701
              factor FootSecond =
                29.9213
              v' =
                factor v
              w' =
                factor w
          in a * v' / w'
        )
        w
    )
