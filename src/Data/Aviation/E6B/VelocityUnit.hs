{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.E6B.VelocityUnit(
  VelocityUnit(..)
, HasVelocityUnit(..)
, AsVelocityUnit(..)
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
