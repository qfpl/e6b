{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.E6B.PressureUnit(
  PressureUnit(..)
, HasPressureUnit(..)
, AsPressureUnit(..)
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
