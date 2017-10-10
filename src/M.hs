{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK prune #-}

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
  TrueAirspeedDensityAltitude
    TrueAirspeed
    DensityAltitude
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

data IndicatedAirSpeed =
  IndicatedAirSpeed
    Double
  deriving (Eq, Ord, Show)

class HasIndicatedAirSpeed a where
  indicatedAirSpeed ::
    Lens'
      a
      IndicatedAirSpeed

instance HasIndicatedAirSpeed IndicatedAirSpeed where
  indicatedAirSpeed =
    id

instance HasIndicatedAirSpeed Double where
  indicatedAirSpeed =
    from _Wrapped

data PressureAltitudeTemperatureAirSpeed =
  PressureAltitudeTemperatureAirSpeed
    PressureAltitude
    Temperature
    IndicatedAirSpeed
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

instance HasIndicatedAirSpeed PressureAltitudeTemperatureAirSpeed where
  indicatedAirSpeed =
    lens
      (\(PressureAltitudeTemperatureAirSpeed _ _ a) -> a)
      (\(PressureAltitudeTemperatureAirSpeed p t _) a -> PressureAltitudeTemperatureAirSpeed p t a)

data Altitude =
  Altitude
    Double
  deriving (Eq, Ord, Show)

class HasAltitude a where
  altitude ::
    Lens'
      a
      Altitude

instance HasAltitude Altitude where
  altitude =
    id

instance HasAltitude Double where
  altitude =
    from _Wrapped

data QNH =
  QNH
    Double
  deriving (Eq, Ord, Show)

class HasQNH a where
  qnh ::
    Lens'
      a
      QNH

instance HasQNH QNH where
  qnh =
    id

instance HasQNH Double where
  qnh =
    from _Wrapped

data AltitudeQNHTemperature =
  AltitudeQNHTemperature
    Altitude
    QNH
    Temperature
  deriving (Eq, Ord, Show)

instance HasAltitude AltitudeQNHTemperature where
  altitude =
    lens
      (\(AltitudeQNHTemperature a _ _) -> a)
      (\(AltitudeQNHTemperature _ q t) a -> AltitudeQNHTemperature a q t)

instance HasQNH AltitudeQNHTemperature where
  qnh =
    lens
      (\(AltitudeQNHTemperature _ q _) -> q)
      (\(AltitudeQNHTemperature a _ t) q -> AltitudeQNHTemperature a q t)

instance HasTemperature AltitudeQNHTemperature where
  temperature =
    lens
      (\(AltitudeQNHTemperature _ _ t) -> t)
      (\(AltitudeQNHTemperature a q _) t -> AltitudeQNHTemperature a q t)

data DensityAltitudePressureAltitude =
  DensityAltitudePressureAltitude
    DensityAltitude
    PressureAltitude
  deriving (Eq, Ord, Show)

instance HasDensityAltitude DensityAltitudePressureAltitude where
  densityAltitude =
    lens
      (\(DensityAltitudePressureAltitude d _) -> d)
      (\(DensityAltitudePressureAltitude _ p) d -> DensityAltitudePressureAltitude d p)

instance HasPressureAltitude DensityAltitudePressureAltitude where
  pressureAltitude =
    lens
      (\(DensityAltitudePressureAltitude _ p) -> p)
      (\(DensityAltitudePressureAltitude d _) p -> DensityAltitudePressureAltitude d p)

makeWrapped ''TrueAirspeed
makeWrapped ''DensityAltitude
makeWrapped ''PressureAltitude
makeWrapped ''Temperature
makeWrapped ''IndicatedAirSpeed
makeClassy ''TrueAirspeedDensityAltitude
makeClassy ''PressureAltitudeTemperatureAirSpeed
makeWrapped ''Altitude
makeWrapped ''QNH
makeClassy ''AltitudeQNHTemperature
makeClassy ''DensityAltitudePressureAltitude

calculateTrueAirspeedDensityAltitude ::
  (HasIndicatedAirSpeed x, HasTemperature x, HasPressureAltitude x) =>
  x
  -> TrueAirspeedDensityAltitude
calculateTrueAirspeedDensityAltitude =
  calculateTrueAirspeedDensityAltitude' pressureAltitude temperature indicatedAirSpeed

calculateTrueAirspeedDensityAltitude' ::
  (
    Unwrapped palt ~ Unwrapped temp
  , Unwrapped temp ~ Double
  , Unwrapped ias ~ Unwrapped palt
  , Rewrapped palt palt
  , Rewrapped ias ias
  , Rewrapped temp temp
  ) =>
  Getting palt x palt
  -> Getting temp x temp
  -> Getting ias x ias
  -> x
  -> TrueAirspeedDensityAltitude
calculateTrueAirspeedDensityAltitude' getPressureAltitude getTemperature getIndicatedAirSpeed pa_oat_ias =
  let palt = pa_oat_ias ^. getPressureAltitude
      tmp = pa_oat_ias ^. getTemperature
      ias = pa_oat_ias ^. getIndicatedAirSpeed
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

calculateDensityAltitudePressureAltitude ::
  (HasAltitude s, HasQNH s, HasTemperature s) =>
  s
  -> DensityAltitudePressureAltitude
calculateDensityAltitudePressureAltitude =
  calculateDensityAltitudePressureAltitude' altitude qnh temperature

calculateDensityAltitudePressureAltitude' ::
  (
    Unwrapped alt ~ Double
  , Unwrapped qnh ~ Double
  , Unwrapped temp ~ Double
  , Rewrapped alt alt
  , Rewrapped qnh qnh
  , Rewrapped temp temp) =>
  Getting alt x alt
  -> Getting qnh x qnh
  -> Getting temp x temp
  -> x
  -> DensityAltitudePressureAltitude
calculateDensityAltitudePressureAltitude' getAltitude getQNH getTemperature alt_qnh_temp =
  let alt = alt_qnh_temp ^. getAltitude
      q = alt_qnh_temp ^. getQNH
      tmp = alt_qnh_temp ^. getTemperature
      ea = ((q ^. _Wrapped) ** 0.1903) - (1.313e-5 * (alt ^. _Wrapped))
      pr = ea ** 5.255
      isatemp = (pr / 29.92) / (tmp ^. _Wrapped / 288.15)
      da = 145442.156 * (1 - (isatemp ** 0.234969))
      pa = ((29.92 ** 0.1903) - ea) / 1.313e-5
  in  DensityAltitudePressureAltitude (_Wrapped # da) (_Wrapped # pa)
