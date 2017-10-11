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
  | Nanometre
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
        1.852
      factor Metre =
        1852
      factor Centimetre =
        185200
      factor Millimetre =
        1852000
      factor Micrometre =
        1852000000
      factor Nanometre =
        1852000000000
      factor StatuteMile =
        1.15078
      factor Yard =
        2025.3728007
      factor Foot =
        6076.118402099979903
      factor Inch =
        72913.420825199762476
      factor NauticalMile =
        1
      f' =
        factor f
      t' =
        factor t
  in  a * f' / t'
