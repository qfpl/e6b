{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module M where

import Papa

newtype TrueAirspeed =
  TrueAirspeed
    Double
  deriving (Eq, Ord, Show)

class HasTrueAirspeed a where
  trueAirspeed ::
    Lens'
      a
      TrueAirspeed

instance HasTrueAirspeed TrueAirspeed where
  trueAirspeed =
    id      

instance HasTrueAirspeed Double where
  trueAirspeed =
    from _Wrapped

newtype DensityAltitude =
  DensityAltitude
    Double
  deriving (Eq, Ord, Show)

class HasDensityAltitude a where
  densityAltitude ::
    Lens'
      a
      DensityAltitude

instance HasDensityAltitude DensityAltitude where
  densityAltitude =
    id      

instance HasDensityAltitude Double where
  densityAltitude =
    from _Wrapped

data TrueAirspeedDensityAltitude =
  TrueAirspeedDensityAltitude {
    tas ::
      TrueAirspeed
  , da ::
      DensityAltitude
  }
  deriving (Eq, Ord, Show)

instance HasTrueAirspeed TrueAirspeedDensityAltitude where
  trueAirspeed =
    lens
      (\(TrueAirspeedDensityAltitude t _) -> t)
      (\(TrueAirspeedDensityAltitude _ d) t -> TrueAirspeedDensityAltitude t d)

instance HasDensityAltitude TrueAirspeedDensityAltitude where
  densityAltitude =
    lens
      (\(TrueAirspeedDensityAltitude _ d) -> d)
      (\(TrueAirspeedDensityAltitude t _) d -> TrueAirspeedDensityAltitude t d)

data PressureAltitude =
  PressureAltitude
    Double
  deriving (Eq, Ord, Show)
    
class HasPressureAltitude a where
  pressureAltitude ::
    Lens'
      a
      PressureAltitude

instance HasPressureAltitude Double where
  pressureAltitude =
    from _Wrapped

instance HasPressureAltitude PressureAltitude where
  pressureAltitude =
    id      

data Temperature =
  Temperature
    Double
  deriving (Eq, Ord, Show)

class HasTemperature a where
  temperature ::
    Lens'
      a
      Temperature

instance HasTemperature Temperature where
  temperature =
    id      

instance HasTemperature Double where
  temperature =
    from _Wrapped

data AirSpeed =
  AirSpeed
    Double
  deriving (Eq, Ord, Show)

class HasAirSpeed a where
  airSpeed ::
    Lens'
      a
      AirSpeed

instance HasAirSpeed AirSpeed where
  airSpeed =
    id

instance HasAirSpeed Double where
  airSpeed =
    from _Wrapped

data PressureAltitudeTemperatureAirSpeed =
  PressureAltitudeTemperatureAirSpeed {
    pa ::
      PressureAltitude
  , temp ::
      Temperature
  , airspeed ::
      AirSpeed
  }
  deriving (Eq, Ord, Show)

instance HasPressureAltitude PressureAltitudeTemperatureAirSpeed where
  pressureAltitude =
    lens
      (\(PressureAltitudeTemperatureAirSpeed p _ _) -> p)
      (\(PressureAltitudeTemperatureAirSpeed _ t a) p -> PressureAltitudeTemperatureAirSpeed p t a)

instance HasTemperature PressureAltitudeTemperatureAirSpeed where
  temperature =
    lens
      (\(PressureAltitudeTemperatureAirSpeed _ t _) -> t)
      (\(PressureAltitudeTemperatureAirSpeed p _ a) t -> PressureAltitudeTemperatureAirSpeed p t a)

instance HasAirSpeed PressureAltitudeTemperatureAirSpeed where
  airSpeed =
    lens
      (\(PressureAltitudeTemperatureAirSpeed _ _ a) -> a)
      (\(PressureAltitudeTemperatureAirSpeed p t _) a -> PressureAltitudeTemperatureAirSpeed p t a)

makeWrapped ''TrueAirspeed
makeWrapped ''DensityAltitude
makeWrapped ''PressureAltitude
makeWrapped ''Temperature
makeWrapped ''AirSpeed
makeClassy ''TrueAirspeedDensityAltitude
makeClassy ''PressureAltitudeTemperatureAirSpeed

calculateTrueAirspeedDensityAltitude ::
  (HasAirSpeed x, HasTemperature x, HasPressureAltitude x) =>
  x
  -> TrueAirspeedDensityAltitude
calculateTrueAirspeedDensityAltitude =
  calculateTrueAirspeedDensityAltitude' pressureAltitude temperature airSpeed

calculateTrueAirspeedDensityAltitude' ::
  (
    Unwrapped temp ~ Double
  , Unwrapped palt ~ Unwrapped temp
  , Unwrapped ias ~ Unwrapped palt
  , Rewrapped ias ias
  , Rewrapped palt palt
  , Rewrapped temp temp
  ) =>
  Getting palt x palt
  -> Getting temp x temp
  -> Getting ias x ias
  -> x
  -> TrueAirspeedDensityAltitude
calculateTrueAirspeedDensityAltitude' getPressureAltitude getTemperature getAirSpeed pa_oat_ias =
  let palt =  pa_oat_ias ^. getPressureAltitude
      tmp =  pa_oat_ias ^. getTemperature
      ias =  pa_oat_ias ^. getAirSpeed
      speed_of_sound = 661.4788 -- knots
      pa_sealevel = 29.92 ** 0.1903 -- Pressure Altitude with constant sea level
      p = (pa_sealevel - 1.313e-5 * palt ^. _Wrapped) ** 5.255
      dalt = (1 - ((p / 29.92) / (tmp ^. _Wrapped / 288.15)) ** 0.234969) * 145442.156
      soundspeed = ias ^. _Wrapped / speed_of_sound
      css = (soundspeed * soundspeed) / 5 + 1
      qc = (css ** 3.5 - 1) * 29.92126 -- impact pressure on pitot tube
      mach = sqrt (5 * ((qc / p + 1) ** (2/7) - 1))
      es = speed_of_sound * mach -- effective airspeed
      ts = es * sqrt ((tmp ^. _Wrapped) / 288.15)
  in  TrueAirspeedDensityAltitude (_Wrapped # ts) (_Wrapped # dalt)
