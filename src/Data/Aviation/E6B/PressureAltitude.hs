{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.E6B.PressureAltitude(
  PressureUnit(..)
, PressureAltitude(..)
, lensPressureAltitude
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit(Pascal, InHg, Psi, Torr, Atmosphere, Bar))
import Papa

data PressureAltitude a =
  PressureAltitude
    a
    PressureUnit
  deriving (Eq, Ord, Show)

lensPressureAltitude ::
  Fractional a =>
  Lens'
    (PressureAltitude a)
    PressureUnit
lensPressureAltitude =
  lens
    (\(PressureAltitude _ u) -> u)
    (\(PressureAltitude a v) w ->
      PressureAltitude
        (
          let factor ::
                Fractional a =>
                PressureUnit
                -> a
              factor Pascal =
                0.00029529983071445
              factor InHg =
                1
              factor Psi =
                2.036025
              factor Torr =
                0.0393701
              factor Atmosphere =
                29.9213
              factor Bar =
                29.530027130151
              v' =
                factor v
              w' =
                factor w
          in a * v' / w'
        )
        w
    )
