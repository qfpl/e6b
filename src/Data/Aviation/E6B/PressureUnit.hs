{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.E6B.PressureUnit(
  PressureUnit(..)
, HasPressureUnit(..)
, AsPressureUnit(..)
, factorPressureUnit
) where

import Papa

data PressureUnit =
  Pascal
  | InHg
  | Psi
  | Torr
  | Atmosphere
  | Bar
  deriving (Eq, Ord, Show)

makeClassy ''PressureUnit
makeClassyPrisms ''PressureUnit

factorPressureUnit ::
  Fractional a =>
  PressureUnit
  -> PressureUnit
  -> a
  -> a
factorPressureUnit f t a =
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
      f' =
        factor f
      t' =
        factor t
  in  a * f' / t'
