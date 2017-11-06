{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Main(
  main
) where

import Data.Aviation.E6B
import Options.Applicative(fullDesc, progDesc, header, execParser, info, helper)
import Papa
import System.Console.ANSI
import Text.Printf

printDensityAltitudePressureAltitudeResult2 ::
  (HasTemperature s Double, HasIndicatedAltitude s Double, HasQNH s Double) =>
  Preferences
  -> s
  -> [Either [SGR] String]
printDensityAltitudePressureAltitudeResult2 prefs x =
  let g ::
        DensityAltitudePressureAltitude Double Double
      g =
        calculateDensityAltitudePressureAltitude x
      pr Nothing =
        show
      pr (Just f) =
        printf f
      ia =
        set convertIndicatedAltitude (prefs ^. distance_unit_output) (x ^. indicatedAltitude)
      qnh =
        set convertQNH (prefs ^. pressure_unit_output) (x ^. qNH)
      tp =
        set convertTemperature (prefs ^. temperature_unit_output) (x ^. temperature)
      da =
        set convertDensityAltitude (prefs ^. distance_unit_output) (g ^. densityAltitude)
      pa =
        set convertPressureAltitude (prefs ^. distance_unit_output) (g ^. pressureAltitude)
  in  
      [
        Left $ prefs ^. output_top_line
      , Right "================================\n"
      , Left $ prefs ^. input_field_name
      , Right "alt:"
      , Left []
      , Right "   "
      , Left $ prefs ^. input_field_value
      , Right $ pr (prefs ^. indicated_altitude_format) (ia ^. indicatedAltitudeValue)
      , Left $ prefs ^. input_field_unit
      , Right $ prefs ^. distance_unit (ia ^. distanceUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. input_field_name
      , Right "qnh:"
      , Left []
      , Right "   "
      , Left $ prefs ^. input_field_value
      , Right $ pr (prefs ^. qnh_format) (qnh ^. qnhValue)
      , Left $ prefs ^. input_field_unit
      , Right $ prefs ^. pressure_unit (qnh ^. pressureUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. input_field_name
      , Right "temp:"
      , Left []
      , Right "  "
      , Left $ prefs ^. input_field_value
      , Right $ pr (prefs ^. temperature_format) (tp ^. temperatureValue)
      , Left $ prefs ^. input_field_unit
      , Right $ prefs ^. temperature_unit (tp ^. temperatureUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. output_centre_line
      , Right "--------------------------------\n"
      , Left $ prefs ^. output_field_name
      , Right "d.alt:"
      , Left []
      , Right " "
      , Left $ prefs ^. output_field_value
      , Right $ pr (prefs ^. density_altitude_format) (da ^. densityAltitudeValue)
      , Left $ prefs ^. output_field_unit
      , Right $ prefs ^. distance_unit (da ^. distanceUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. output_field_name
      , Right "p.alt:"
      , Left []
      , Right " "
      , Left $ prefs ^. output_field_value
      , Right $ pr (prefs ^. pressure_altitude_format) (pa ^. pressureAltitudeValue)
      , Left $ prefs ^. output_field_unit
      , Right $ prefs ^. distance_unit (pa ^. distanceUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. output_top_line
      , Right "================================\n"
      , Left []
      ]

printTrueAirspeedDensityAltitudeResult2 ::
  (HasIndicatedAirspeed s Double, HasTemperature s Double, HasPressureAltitude s Double) =>
  Preferences
  -> s
  -> [Either [SGR] String]
printTrueAirspeedDensityAltitudeResult2 prefs x =
  let g ::
        TrueAirspeedDensityAltitude Double Double
      g =
        calculateTrueAirspeedDensityAltitude x
      pr Nothing =
        show
      pr (Just f) =
        printf f
      ias =
        set convertIndicatedAirspeed (prefs ^. velocity_unit_output) (x ^. indicatedAirspeed)
      pa =
        set convertPressureAltitude (prefs ^. distance_unit_output) (x ^. pressureAltitude)
      tp =
        set convertTemperature (prefs ^. temperature_unit_output) (x ^. temperature)
      tas =
        set convertTrueAirspeed (prefs ^. velocity_unit_output) (g ^. trueAirspeed)
      da =
        set convertDensityAltitude (prefs ^. distance_unit_output) (g ^. densityAltitude)
  in  [
        Left $ prefs ^. output_top_line
      , Right "================================\n"
      , Left $ prefs ^. input_field_name
      , Right "ias:"
      , Left []
      , Right "   "
      , Left $ prefs ^. input_field_value
      , Right $ pr (prefs ^. indicated_airspeed_format) (ias ^. indicatedAirspeedValue)
      , Left $ prefs ^. input_field_unit
      , Right $ prefs ^. velocity_unit (ias ^. velocityUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. input_field_name
      , Right "p.alt:"
      , Left []
      , Right " "
      , Left $ prefs ^. input_field_value
      , Right $ pr (prefs ^. pressure_altitude_format) (pa ^. pressureAltitudeValue)
      , Left $ prefs ^. input_field_unit
      , Right $ prefs ^. distance_unit (pa ^. distanceUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. input_field_name
      , Right "temp:"
      , Left []
      , Right "  "
      , Left $ prefs ^. input_field_value
      , Right $ pr (prefs ^. temperature_format) (tp ^. temperatureValue)
      , Left $ prefs ^. input_field_unit
      , Right $ prefs ^. temperature_unit (tp ^. temperatureUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. output_centre_line
      , Right "--------------------------------\n"
      , Left $ prefs ^. output_field_name
      , Right "tas:"
      , Left []
      , Right " "
      , Left $ prefs ^. output_field_value
      , Right $ pr (prefs ^. true_airspeed_format) (tas ^. trueAirspeedValue)
      , Left $ prefs ^. output_field_unit
      , Right $ prefs ^. velocity_unit (tas ^. velocityUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. output_field_name
      , Right "d.alt:"
      , Left []
      , Right " "
      , Left $ prefs ^. output_field_value
      , Right $ pr (prefs ^. density_altitude_format) (da ^. densityAltitudeValue)
      , Left $ prefs ^. output_field_unit
      , Right $ prefs ^. distance_unit (da ^. distanceUnit)
      , Left []
      , Right "\n"
      , Left $ prefs ^. output_top_line
      , Right "================================\n"
      , Left []
      ]

printDensityAltitudePressureAltitudeResult ::
  (HasTemperature s Double, HasIndicatedAltitude s Double, HasQNH s Double) =>
  s
  -> String
printDensityAltitudePressureAltitudeResult x =
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

printTrueAirspeedDensityAltitudeResult ::
  (HasIndicatedAirspeed s Double, HasTemperature s Double, HasPressureAltitude s Double) =>
  s
  -> String
printTrueAirspeedDensityAltitudeResult x =
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

printResult ::
  E6BOptions Double Double Double Double Double
  -> String
printResult (IsIndicatedAltitudeQNHTemperature x) =
  printDensityAltitudePressureAltitudeResult x
printResult (IsIndicatedAirspeedPressureAltitudeTemperature x) =
  printTrueAirspeedDensityAltitudeResult x

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
