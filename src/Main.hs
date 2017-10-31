{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.E6B -- (parserE6BOptions, parseDouble)
import Options.Applicative(fullDesc, progDesc, header, execParser, info, helper)
import Papa
import System.Console.ANSI
import Text.Printf

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
        Nothing
     )
    (
      OutputUnits
        Nothing
        Nothing
        Nothing
        Nothing
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
  Preferences
    SGRs
    PrintfFormats
    OutputUnits
    UnitAbbreviations
  deriving (Eq, Ord, Show)

data SGRs =
  SGRs
    [SGR] -- input field name
    [SGR] -- input field value
    [SGR] -- input field unit
    [SGR] -- output field name
    [SGR] -- output field value
    [SGR] -- output field unit
    [SGR] -- output top line
    [SGR] -- output centre line
    [SGR] -- output bottom line
  deriving (Eq, Ord, Show)

data PrintfFormats =
  PrintfFormats
    (Maybe String) -- input field name
    (Maybe String) -- input field value
    (Maybe String) -- input field unit
    (Maybe String) -- output field name
    (Maybe String) -- output field value
    (Maybe String) -- output field unit
    (Maybe String) -- output top line
    (Maybe String) -- output centre line
    (Maybe String) -- output bottom line
  deriving (Eq, Ord, Show)

data OutputUnits =
  OutputUnits
    (Maybe TemperatureUnit)
    (Maybe DistanceUnit)
    (Maybe VelocityUnit)
    (Maybe PressureUnit)
  deriving (Eq, Ord, Show)

data UnitAbbreviations =
  UnitAbbreviations
    TemperatureUnitAbbreviations
    DistanceUnitAbbreviations
    VelocityUnitAbbreviations
    PressureUnitAbbreviations
  deriving (Eq, Ord, Show)

data TemperatureUnitAbbreviations =
  TemperatureUnitAbbreviations
    String-- celsius
    String -- fahrenheit
    String -- kelvin
  deriving (Eq, Ord, Show)

data DistanceUnitAbbreviations =
  DistanceUnitAbbreviations
    String -- kilometre
    String -- metre
    String -- centimetre
    String -- millimetre
    String -- micrometre
    String -- statute mile
    String -- yard
    String -- foot
    String -- inch
    String -- nautical mile
  deriving (Eq, Ord, Show)

data VelocityUnitAbbreviations =
  VelocityUnitAbbreviations
    String -- knot
    String -- kilometres/hour
    String -- statute miles/hour
    String -- metres/second
    String -- foot/second
  deriving (Eq, Ord, Show)

data PressureUnitAbbreviations =
  PressureUnitAbbreviations
    String -- pascal
    String -- hectopascal
    String -- in/hg
    String -- psi
    String -- torr
    String -- atmosphere
    String -- bar
  deriving (Eq, Ord, Show)

printResult ::
  E6BOptions Double Double Double Double Double
  -> String
printResult (IsIndicatedAltitudeQNHTemperature x) =
  let g ::
        DensityAltitudePressureAltitude Double Double
      g =
        calculateDensityAltitudePressureAltitude x
  in  concat
        [
          "================================"
        , "\n"
        , "alt:   "
        , printDouble (x ^. indicatedAltitude . indicatedAltitudeValue)
        , printDistanceUnit (x ^. indicatedAltitude . distanceUnit)
        , "\n"
        , "qnh:   "
        , printDouble (x ^. qNH . qnhValue)
        , printPressureUnit (x ^. qNH . pressureUnit)
        , "\n"
        , "temp:  "
        , printDouble (x ^. temperature . temperatureValue)
        , printTemperatureUnit (x ^. temperature . temperatureUnit)
        , "\n"
        , "--------------------------------"
        , "\n"
        , "d.alt: "
        , printDouble (g ^. densityAltitude . densityAltitudeValue)
        , printDistanceUnit (g ^. densityAltitude . distanceUnit)
        , "\n"
        , "p.alt: "
        , printDouble (g ^. pressureAltitude . pressureAltitudeValue)
        , printDistanceUnit (g ^. pressureAltitude . distanceUnit)
        , "\n"
        , "================================"
        , "\n"
        ]
printResult (IsIndicatedAirspeedPressureAltitudeTemperature x) =
  let g ::
        TrueAirspeedDensityAltitude Double Double
      g =
        calculateTrueAirspeedDensityAltitude x
  in  concat
        [
          "================================"
        , "\n"
        , "ias:   "
        , printDouble (x ^. indicatedAirspeed . indicatedAirspeedValue)
        , printVelocityUnit (x ^. indicatedAirspeed . velocityUnit)
        , "\n"
        , "p.alt: "
        , printDouble (x ^. pressureAltitude . pressureAltitudeValue)
        , printDistanceUnit (x ^. pressureAltitude . distanceUnit)
        , "\n"
        , "temp:  "
        , printDouble (x ^. temperature . temperatureValue)
        , printTemperatureUnit (x ^. temperature . temperatureUnit)
        , "\n"
        , "--------------------------------"
        , "\n"
        , "tas:   "
        , printDouble (g ^. trueAirspeed . trueAirspeedValue)
        , printVelocityUnit (g ^. trueAirspeed . velocityUnit)
        , "\n"
        , "d.alt: "
        , printDouble (g ^. densityAltitude . densityAltitudeValue)
        , printDistanceUnit (g ^. densityAltitude . distanceUnit)
        , "\n"
        , "================================"
        , "\n"
        ]

main ::
  IO ()
main =
  let im = fullDesc <> progDesc "e6b flight computer functions for aviation" <> header "e6b flight computer"
  in  do  q <- execParser (info (parserE6BOptions parseDouble parseDouble parseDouble parseDouble parseDouble <**> helper) im)
          putStrLn (printResult q)
          
printDouble ::
  Double
  -> String
printDouble =
  printf "%.2f"

printDistanceUnit ::
  DistanceUnit
  -> String
printDistanceUnit Kilometre =
  "km"
printDistanceUnit Metre =
  "m"
printDistanceUnit Centimetre =
  "cm"
printDistanceUnit Millimetre =
  "mm"
printDistanceUnit Micrometre =
  "μm"
printDistanceUnit StatuteMile =
  "sm"
printDistanceUnit Yard =
  "yd"
printDistanceUnit Foot =
  "ft"
printDistanceUnit Inch =
  "in"
printDistanceUnit NauticalMile =
  "nm"

printPressureUnit ::
  PressureUnit
  -> String
printPressureUnit Pascal =
  "Pa"
printPressureUnit Hectopascal =
  "hPa"
printPressureUnit InHg =
  "inHg"
printPressureUnit Psi =
  "psi"
printPressureUnit Torr =
  "Torr"
printPressureUnit Atmosphere =
  "ATM"
printPressureUnit Bar =
  "bar"

printVelocityUnit ::
  VelocityUnit
  -> String
printVelocityUnit Knot =
  "kt"
printVelocityUnit KilometreHour =
  "km/h"
printVelocityUnit StatuteMileHour =
  "mph"
printVelocityUnit MetreSecond =
  "m/s"
printVelocityUnit FootSecond =
  "ft/s"

printTemperatureUnit ::
  TemperatureUnit
  -> String
printTemperatureUnit Celsius =
  "°C"
printTemperatureUnit Fahrenheit =
  "°F"
printTemperatureUnit Kelvin =
  "°K"
