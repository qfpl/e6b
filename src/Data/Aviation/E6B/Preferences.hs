{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aviation.E6B.Preferences where

import Control.Applicative(liftA2)
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Aviation.E6B.TemperatureUnit
import Data.Aviation.E6B.DistanceUnit
import Data.Aviation.E6B.VelocityUnit
import Data.Aviation.E6B.PressureUnit
import Papa
import System.Console.ANSI

newtype PreferencesReaderT f a =
  PreferencesReaderT (Preferences -> f a)

type PreferencesReader a =
  PreferencesReaderT Identity a

instance Functor f => Functor (PreferencesReaderT f) where
  fmap f (PreferencesReaderT k) =
    PreferencesReaderT (fmap f . k)

instance Applicative f => Applicative (PreferencesReaderT f) where
  pure =
    PreferencesReaderT . pure . pure
  PreferencesReaderT f <*> PreferencesReaderT a =
    PreferencesReaderT (liftA2 (<*>) f a)

instance Monad f => Monad (PreferencesReaderT f) where
  return = 
    PreferencesReaderT . return . return
  PreferencesReaderT k >>= f =
    PreferencesReaderT (\p -> k p >>= \a -> let PreferencesReaderT i = f a in i p)

instance MonadTrans PreferencesReaderT where
  lift =
    PreferencesReaderT . pure

defaultPreferences ::
  Preferences
defaultPreferences =
  Preferences
    (
      SGRs
        []
        []
        []
        []
        []
        []
        []
        []
        []
    )
    (
      PrintfFormats
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
     )
    (
      OutputUnits
        Celsius
        Metre
        KilometreHour
        Hectopascal
    )
    (
      UnitAbbreviations
        (
          TemperatureUnitAbbreviations
            "°C"
            "°F"
            "°K"
        )
        (
          DistanceUnitAbbreviations
            "km"
            "m"
            "cm"
            "mm"
            "μm"
            "sm"
            "yd"
            "ft"
            "in"
            "nm"
        )
        (
          VelocityUnitAbbreviations
            "kt"
            "km/h"
            "mph"
            "m/s"
            "ft/s"
        )
        (
          PressureUnitAbbreviations
            "Pa"
            "hPa"
            "inHg"
            "psi"
            "Torr"
            "ATM"
            "bar"
        )
    )

data Preferences =
  Preferences {
    _sgrs ::
      SGRs
  , _printf_formats ::
       PrintfFormats
  , _output_units ::
       OutputUnits
  , _unit_abbreviations ::
       UnitAbbreviations
  } deriving (Eq, Ord, Show)

data SGRs =
  SGRs {
    _input_field_name ::
      [SGR] -- input field name
  , _input_field_value ::
      [SGR] -- input field value
  , _input_field_unit ::
      [SGR] -- input field unit
  , _output_field_name ::
      [SGR] -- output field name
  , _output_field_value ::
      [SGR] -- output field value
  , _output_field_unit ::
      [SGR] -- output field unit
  , _output_top_line ::
      [SGR] -- output top line
  , _output_centre_line ::
      [SGR] -- output centre line
  , _output_bottom_line ::
      [SGR] -- output bottom line
  } deriving (Eq, Ord, Show)

data PrintfFormats =
  PrintfFormats {
    _indicated_altitude_format ::
      Maybe String -- indicated altitude
  , _density_altitude_format ::
      Maybe String -- density altitude
  , _pressure_altitude_format ::
      Maybe String -- pressure altitude
  , _qnh_format ::
      Maybe String -- qnh
  , _temperature_format ::
      Maybe String -- temperature
  , _indicated_airspeed ::
      Maybe String -- indicated airspeed
  , _true_airspeed ::
      Maybe String -- true airspeed
  , _density_altitude ::
      Maybe String -- density altitude
  } deriving (Eq, Ord, Show)

data OutputUnits =
  OutputUnits {
    _temperature_unit_output ::
      TemperatureUnit
  , _distance_unit_output ::
      DistanceUnit
  , _velocity_unit_output ::
      VelocityUnit
  , _pressure_unit_output ::
      PressureUnit
  } deriving (Eq, Ord, Show)

data UnitAbbreviations =
  UnitAbbreviations {
    _temperature_unit_abbreviations ::
      TemperatureUnitAbbreviations
  , _distance_unit_abbreviations ::
      DistanceUnitAbbreviations
  , _velocity_unit_abbreviations ::
     VelocityUnitAbbreviations
  , _pressure_unit_abbreviations ::
      PressureUnitAbbreviations
  } deriving (Eq, Ord, Show)

data TemperatureUnitAbbreviations =
  TemperatureUnitAbbreviations {
    _temperature_unit_celsius ::
      String-- celsius
  , _temperature_unit_fahrenheit ::
      String -- fahrenheit
  , _temperature_unit_kelvin ::
      String -- kelvin
  } deriving (Eq, Ord, Show)

data DistanceUnitAbbreviations =
  DistanceUnitAbbreviations {
    _distance_unit_kilometre ::
      String -- kilometre
  , _distance_unit_metre ::
      String -- metre
  , _distance_unit_centimetre ::
      String -- centimetre
  , _distance_unit_millimetre ::
      String -- millimetre
  , _distance_unit_micrometre ::
      String -- micrometre
  , _distance_unit_statutemile ::
      String -- statute mile
  , _distance_unit_yard ::
      String -- yard
  , _distance_unit_foot ::
      String -- foot
  , _distance_unit_inch ::
      String -- inch
  , _distance_unit_nauticalmile ::
      String -- nautical mile
  } deriving (Eq, Ord, Show)

data VelocityUnitAbbreviations =
  VelocityUnitAbbreviations {
    _velocity_unit_knot ::
      String -- knot
  , _velocity_unit_kilometrehour ::
      String -- kilometres/hour
  , _velocity_unit_statutemilehour ::
      String -- statute miles/hour
  , _velocity_unit_metresecond ::
      String -- metres/second
  , _velocity_unit_footsecond ::
      String -- foot/second
  } deriving (Eq, Ord, Show)

data PressureUnitAbbreviations =
  PressureUnitAbbreviations {
    _pressure_unit_pascal ::
      String -- pascal
  , _pressure_unit_hectopascal ::
      String -- hectopascal
  , _pressure_unit_inhg ::
      String -- in/hg
  , _pressure_unit_psi ::
      String -- psi
  , _pressure_unit_torr ::
      String -- torr
  , _pressure_unit_atmosphere ::
      String -- atmosphere
  , _pressure_unit_bar ::
      String -- bar
  } deriving (Eq, Ord, Show)

makeClassy ''Preferences
makeClassy ''SGRs
makeClassy ''PrintfFormats
makeClassy ''OutputUnits
makeClassy ''UnitAbbreviations
makeClassy ''TemperatureUnitAbbreviations
makeClassy ''DistanceUnitAbbreviations
makeClassy ''VelocityUnitAbbreviations
makeClassy ''PressureUnitAbbreviations

undefined = undefined

instance HasSGRs Preferences where
  sGRs =
    sgrs . sGRs

instance HasPrintfFormats Preferences where
  printfFormats =
    printf_formats . printfFormats

instance HasOutputUnits Preferences where
  outputUnits =
    output_units . outputUnits

instance HasUnitAbbreviations Preferences where
  unitAbbreviations =
    unit_abbreviations . unitAbbreviations

instance HasTemperatureUnit OutputUnits where
  temperatureUnit =
    temperature_unit_output . temperatureUnit

instance HasDistanceUnit OutputUnits where
  distanceUnit =
    distance_unit_output . distanceUnit

instance HasVelocityUnit OutputUnits where
  velocityUnit =
    velocity_unit_output . velocityUnit

instance HasPressureUnit OutputUnits where
  pressureUnit =
    pressure_unit_output . pressureUnit

instance HasTemperatureUnit Preferences where
  temperatureUnit =
    output_units . temperatureUnit

instance HasDistanceUnit Preferences where
  distanceUnit =
    output_units . distanceUnit

instance HasVelocityUnit Preferences where
  velocityUnit =
    output_units . velocityUnit

instance HasPressureUnit Preferences where
  pressureUnit =
    output_units . pressureUnit


instance HasTemperatureUnitAbbreviations UnitAbbreviations where
  temperatureUnitAbbreviations =
    temperature_unit_abbreviations . temperatureUnitAbbreviations

instance HasDistanceUnitAbbreviations UnitAbbreviations where
  distanceUnitAbbreviations =
    distance_unit_abbreviations . distanceUnitAbbreviations

instance HasVelocityUnitAbbreviations UnitAbbreviations where
  velocityUnitAbbreviations =
    velocity_unit_abbreviations . velocityUnitAbbreviations

instance HasPressureUnitAbbreviations UnitAbbreviations where
  pressureUnitAbbreviations =
    pressure_unit_abbreviations . pressureUnitAbbreviations

instance HasTemperatureUnitAbbreviations Preferences where
  temperatureUnitAbbreviations =
    unit_abbreviations . temperatureUnitAbbreviations

instance HasDistanceUnitAbbreviations Preferences where
  distanceUnitAbbreviations =
    unit_abbreviations . distanceUnitAbbreviations

instance HasVelocityUnitAbbreviations Preferences where
  velocityUnitAbbreviations =
    unit_abbreviations . velocityUnitAbbreviations

instance HasPressureUnitAbbreviations Preferences where
  pressureUnitAbbreviations =
    unit_abbreviations . pressureUnitAbbreviations

temperature_unit ::
  HasTemperatureUnitAbbreviations c =>
  TemperatureUnit
  -> Lens' c String
temperature_unit Celsius =
  temperature_unit_celsius
temperature_unit Fahrenheit =
  temperature_unit_fahrenheit
temperature_unit Kelvin =
  temperature_unit_kelvin

distance_unit ::
  HasDistanceUnitAbbreviations c =>
  DistanceUnit
  -> Lens' c String
distance_unit Kilometre =
  distance_unit_kilometre
distance_unit Metre =
  distance_unit_metre
distance_unit Centimetre =
  distance_unit_centimetre
distance_unit Millimetre =
  distance_unit_millimetre
distance_unit Micrometre =
  distance_unit_micrometre
distance_unit StatuteMile =
  distance_unit_statutemile
distance_unit Yard =
  distance_unit_yard
distance_unit Foot =
  distance_unit_foot
distance_unit Inch =
  distance_unit_inch
distance_unit NauticalMile =
  distance_unit_nauticalmile

velocity_unit ::
  HasVelocityUnitAbbreviations c =>
  VelocityUnit
  -> Lens' c String
velocity_unit Knot =
  velocity_unit_knot
velocity_unit KilometreHour =
  velocity_unit_kilometrehour
velocity_unit StatuteMileHour =
  velocity_unit_statutemilehour
velocity_unit MetreSecond =
  velocity_unit_metresecond
velocity_unit FootSecond =
  velocity_unit_footsecond

pressure_unit ::
  HasPressureUnitAbbreviations c =>
  PressureUnit
  -> Lens' c String
pressure_unit Pascal =
  pressure_unit_pascal
pressure_unit Hectopascal =
  pressure_unit_hectopascal
pressure_unit InHg =
  pressure_unit_inhg
pressure_unit Psi =
  pressure_unit_psi
pressure_unit Torr =
  pressure_unit_torr
pressure_unit Atmosphere =
  pressure_unit_atmosphere
pressure_unit Bar =
  pressure_unit_bar
