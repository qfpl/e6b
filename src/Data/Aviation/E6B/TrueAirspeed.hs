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
                1
              factor KilometreHour =
                1.852
              factor StatuteMileHour =
                1.15078
              factor MetreSecond =
                0.514444
              factor FootSecond =
                1.687808398944992
              v' =
                factor v
              w' =
                factor w
          in a * v' / w'
        )
        w
    )
