{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.E6B
import Test.Tasty(defaultMain, testGroup)
import Test.Tasty.HUnit(testCase, (@?=))
import Papa

main ::
  IO ()
main =
  defaultMain .
    testGroup "e6b tests" $
      [
        let ias_pa_temp ::
              IndicatedAirspeedPressureAltitudeTemperature Double Double Double
            ias_pa_temp =
              IndicatedAirspeedPressureAltitudeTemperature
                (IndicatedAirspeed 105 Knot)
                (PressureAltitude 1000 Metre)
                (Temperature 25 Celsius)
            tas_da =
              TrueAirspeedDensityAltitude
                (TrueAirspeed 113.35867833252753 Knot)
                (DensityAltitude 5167.015698581208 Foot)
        in testCase "calculate TAS/DA 1" (calculateTrueAirspeedDensityAltitude ias_pa_temp @?= tas_da)
      , let ias_pa_temp ::
              IndicatedAirspeedPressureAltitudeTemperature Double Double Double
            ias_pa_temp =
              IndicatedAirspeedPressureAltitudeTemperature
                (IndicatedAirspeed 120 Knot)
                (PressureAltitude 8000 Foot)
                (Temperature 288.15 Kelvin)
            tas_da =
              TrueAirspeedDensityAltitude
                (TrueAirspeed 139.03717628628445 Knot)
                (DensityAltitude 9812.316830844242 Foot)
        in testCase "calculate TAS/DA 2" (calculateTrueAirspeedDensityAltitude ias_pa_temp @?= tas_da)
      , let ia_qnh_temp ::
              IndicatedAltitudeQNHTemperature Double Double Double
            ia_qnh_temp =
              IndicatedAltitudeQNHTemperature
                (IndicatedAltitude 2000 Foot)
                (QNH 28.92 InHg)
                (Temperature 303.15 Kelvin)
            da_pa =
              DensityAltitudePressureAltitude
                (DensityAltitude 5297.394875934968 Foot)
                (PressureAltitude 2937.6585807120227 Foot)
        in testCase "calculate DA/PA 1" (calculateDensityAltitudePressureAltitude ia_qnh_temp @?= da_pa)
      ]
