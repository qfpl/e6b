module Main where

import Data.Aviation.E6B
import Hedgehog(Gen, forAll, property, assert, (===))
import Prelude
import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog(testProperty)
import Test.Tasty.HUnit(testCase, (@?=))

main ::
  IO ()
main =
  defaultMain $
    testGroup "e6b tests" $
      [
        let ias_pa_temp ::
              IndicatedAirspeedPressureAltitudeTemperature Double Double Double
            ias_pa_temp = IndicatedAirspeedPressureAltitudeTemperature (IndicatedAirspeed 105 Knot) (PressureAltitude 1000 Metre) (Temperature 25 Celsius)
            tas_da = TrueAirspeedDensityAltitude (TrueAirspeed 113.35867833252753 Knot) (DensityAltitude 5167.015698581208 Foot)
        in testProperty "" (property (calculateTrueAirspeedDensityAltitude ias_pa_temp === tas_da))
      -- , testProperty "" (testCase undefined undefined)
      ]
