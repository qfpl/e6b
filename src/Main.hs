{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.E6B.CLI
import Data.Aviation.E6B -- (parserE6BOptions, parseDouble)
import Options.Applicative(fullDesc, progDesc, header, execParser, info, helper)
import Papa
import Text.Printf

undefined = undefined

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

{-

data DensityAltitudePressureAltitude da pa
  = DensityAltitudePressureAltitude (DensityAltitude da)
                                    (PressureAltitude pa)


  :: (HasTemperature s a, HasQNH s a, HasIndicatedAltitude s a,
      Floating a) =>
     s -> DensityAltitudePressureAltitude a a

-}