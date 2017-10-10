{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.QNH(
  QNH(..)
, HasQNH(..)
, qnhValue
, convertQNH
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit, HasPressureUnit(pressureUnit), factorPressureUnit)
import Papa

data QNH a =
  QNH
    a
    PressureUnit
  deriving (Eq, Ord, Show)

makeClassy ''QNH

instance HasPressureUnit (QNH a) where
  pressureUnit =
    lens
      (\(QNH _ u) -> u)
      (\(QNH a _) u -> QNH a u)

qnhValue ::
  Lens
    (QNH a)
    (QNH b)
    a
    b
qnhValue =
  lens
    (\(QNH a _) -> a)
    (\(QNH _ u) a -> QNH a u)

convertQNH ::
  Fractional a =>
  Lens'
    (QNH a)
    PressureUnit
convertQNH =
  lens
    (\(QNH _ u) -> u)
    (\(QNH a v) w -> QNH (factorPressureUnit v w a) w)
