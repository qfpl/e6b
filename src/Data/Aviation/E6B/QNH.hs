{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.E6B.QNH(
  PressureUnit(..)
, QNH(..)
, HasQNH(..)
, lensQNH
) where

import Data.Aviation.E6B.PressureUnit(PressureUnit(Pascal, InHg, Psi, Torr, Atmosphere, Bar), factorPressureUnit)
import Papa

data QNH a =
  QNH
    a
    PressureUnit
  deriving (Eq, Ord, Show)

makeClassy ''QNH

lensQNH ::
  Fractional a =>
  Lens'
    (QNH a)
    PressureUnit
lensQNH =
  lens
    (\(QNH _ u) -> u)
    (\(QNH a v) w -> QNH (factorPressureUnit v w a) w)
