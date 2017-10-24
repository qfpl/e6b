{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.E6B.DistanceUnit(
  DistanceUnit(..)
, HasDistanceUnit(..)
, AsDistanceUnit(..)
, factorDistanceUnit
) where

import Papa

data DistanceUnit =
  Kilometre
  | Metre
  | Centimetre
  | Millimetre
  | Micrometre
  | StatuteMile
  | Yard
  | Foot
  | Inch
  | NauticalMile
  deriving (Eq, Ord, Show)

makeClassy ''DistanceUnit
makeClassyPrisms ''DistanceUnit

factorDistanceUnit ::
  Fractional a =>
  DistanceUnit
  -> DistanceUnit
  -> a
  -> a
factorDistanceUnit f t a =
  let factor ::
        Fractional a =>
        DistanceUnit
        -> a
      factor Kilometre =
        0.539960631749
      factor Metre =
        5.39960631749e-4
      factor Centimetre =
        5.39960631749e-6
      factor Millimetre =
        5.39960631749e-7
      factor Micrometre =
        5.39960631749e-10
      factor StatuteMile =
        0.868976
      factor Yard =
        0.000493737
      factor Foot =
        0.000164579
      factor Inch =
        1.3715e-5
      factor NauticalMile =
        1
      f' =
        factor f
      t' =
        factor t
  in  a * f' / t'
