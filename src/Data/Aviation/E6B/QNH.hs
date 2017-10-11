{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.QNH(
  QNH(..)
, HasQNH(..)
, convertQNH
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit, factorPressureUnit)
import Papa

data QNH a =
  QNH
    a
    PressureUnit
  deriving (Eq, Ord, Show)

makeClassy ''QNH

convertQNH ::
  Fractional a =>
  Lens'
    (QNH a)
    PressureUnit
convertQNH =
  lens
    (\(QNH _ u) -> u)
    (\(QNH a v) w -> QNH (factorPressureUnit v w a) w)
