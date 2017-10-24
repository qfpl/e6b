{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Applicative
import Data.Aviation.E6B
import Data.Foldable
-- import Options.Applicative
import Text.Parser.Combinators(try, optional)
import Text.Parser.Char(CharParsing, spaces, char)
import Text.Parser.Token(TokenParsing, naturalOrDouble)
import Text.Parsec()
import Papa

main ::
  IO ()
main =
  putStrLn "e6b"

parseDensityAltitude' ::
  CharParsing f =>
  f a
  -> f (DensityAltitude a)
parseDensityAltitude' p =
  DensityAltitude <$> p <* spaces <*> parseDistanceUnit

parseDensityAltitude ::
  (TokenParsing f, CharParsing f) =>
  f (DensityAltitude Double)
parseDensityAltitude =
  parseDensityAltitude' parseDouble

parseIndicatedAirspeed' ::
  CharParsing f =>
  f a
  -> f (IndicatedAirspeed a)
parseIndicatedAirspeed' p =
  IndicatedAirspeed <$> p <* spaces <*> parseVelocityUnit

parseIndicatedAirspeed ::
  (TokenParsing f, CharParsing f) =>
  f (IndicatedAirspeed Double)
parseIndicatedAirspeed =
  parseIndicatedAirspeed' parseDouble

parseIndicatedAltitude' ::
  CharParsing f =>
  f a
  -> f (IndicatedAltitude a)
parseIndicatedAltitude' p =
  IndicatedAltitude <$> p <* spaces <*> parseDistanceUnit

parseIndicatedAltitude ::
  (TokenParsing f, CharParsing f) =>
  f (IndicatedAltitude Double)
parseIndicatedAltitude =
  parseIndicatedAltitude' parseDouble

parsePressureAltitude' ::
  CharParsing f =>
  f a
  -> f (PressureAltitude a)
parsePressureAltitude' p =
  PressureAltitude <$> p <* spaces <*> parseDistanceUnit

parsePressureAltitude ::
  (TokenParsing f, CharParsing f) =>
  f (PressureAltitude Double)
parsePressureAltitude =
  parsePressureAltitude' parseDouble

parseQNH' ::
  CharParsing f =>
  f a
  -> f (QNH a)
parseQNH' p =
  QNH <$> p <* spaces <*> parsePressureUnit

parseQNH ::
  (TokenParsing f, CharParsing f) =>
  f (QNH Double)
parseQNH =
  parseQNH' parseDouble

parseTemperature' ::
  CharParsing f =>
  f a
  -> f (Temperature a)
parseTemperature' p =
  Temperature <$> p <* spaces <*> parseTemperatureUnit

parseTemperature ::
  (TokenParsing f, CharParsing f) =>
  f (Temperature Double)
parseTemperature =
  parseTemperature' parseDouble

parseTrueAirspeed' ::
  CharParsing f =>
  f a
  -> f (TrueAirspeed a)
parseTrueAirspeed' p =
  TrueAirspeed <$> p <* spaces <*> parseVelocityUnit

parseTrueAirspeed ::
  (TokenParsing f, CharParsing f) =>
  f (TrueAirspeed Double)
parseTrueAirspeed =
  parseTrueAirspeed' parseDouble

parseDistanceUnit ::
  CharParsing f =>
  f DistanceUnit
parseDistanceUnit =
  parseUnit
    [
      (Kilometre   , ["kilometres", "kilometers", "kilometre", "kilometer", "km"])
    , (Centimetre  , ["centimetres", "centimeters", "centimetre", "centimeter", "cm"])
    , (Micrometre  , ["micrometres", "micrometers", "micrometre", "micrometer", "microns", "micron", "μm"])
    , (StatuteMile , ["miles", "mile", "sm", "mi"])
    , (Yard        , ["yards", "yard", "yds", "yd"])
    , (Foot        , ["foot", "feet", "ft", "'", "’"])
    , (Inch        , ["inches", "inch", "in", "\"", "”"])
    , (NauticalMile, ["nm"])
    , (Millimetre  , ["millimetres", "millimeters", "millimetre", "millimeter", "mm"])
    , (Metre       , ["metres", "meters", "metre", "meter", "m"])
    ]

parsePressureUnit ::
  CharParsing f =>
  f PressureUnit
parsePressureUnit =
  parseUnit
    [
      (Pascal     , ["pascals", "pascal", "pa"])
    , (Hectopascal, ["hectopascals", "hectopascal", "hpa", "millibars", "millibar", "mb", "mbar"])
    , (InHg       , ["inhg", "\"hg", "”hg"])
    , (Psi        , ["psi", "lbf/in²", "lbf/in2", "lb/in", "lbf-in²", "lbf-in2", "lb-in"])
    , (Torr       , ["torrs", "torr"])
    , (Atmosphere , ["atmospheres", "atmosphere", "atm"])
    , (Bar        , ["bar"])
    ]

parseTemperatureUnit ::
  CharParsing f =>
  f TemperatureUnit
parseTemperatureUnit =
  parseUnit
    [
      (Celsius   , ["celsius", "cels", "c", "°c"])
    , (Fahrenheit, ["fahrenheit", "fah", "fahr", "f", "°f"])
    , (Kelvin    , ["kelvin", "k", "°k"])
    ]

parseVelocityUnit ::
  CharParsing f =>
  f VelocityUnit
parseVelocityUnit =
  parseUnit
    [
      (Knot   , ["knots", "knot", "kt", "kts", "kn"])
    , (KilometreHour, ["kph", "km/hr", "kmh", "kp/h", "km/h", "kmph", "k.p.h.", "km/hour"])
    , (StatuteMileHour    , ["mph", "mi/h"])
    , (MetreSecond    , ["m/s", "mps", "m·s⁻¹", "m s⁻¹"])
    , (FootSecond    , ["ft/s", "ft/sec", "fps", "ft s⁻¹"])
    ]

----

parseUnit ::
  (Foldable t, Traversable k, CharParsing f, Monad t) =>
  t (a, t (k Char))
  -> f a
parseUnit r =
  asum
    (r >>= \(x, q) -> q >>= \s -> pure (x <$ try (caseInsensitiveString s)))

parseDouble ::
  (TokenParsing f, CharParsing f) =>
  f Double
parseDouble =
  let sign ::
        Num b =>
        Maybe a
        -> b 
      sign =
        bool 1 (-1) . isJust
  in  (\n d -> sign n * either fromIntegral id d) <$> optional (char '-') <* spaces <*> naturalOrDouble

caseInsensitiveString ::
  (CharParsing f, Traversable t) =>
  t Char
  -> f (t Char)
caseInsensitiveString s =
  let caseInsensitiveChar c =
        char (toLower c) <|> char (toUpper c)
  in  try (mapM caseInsensitiveChar s)

{-
calculateDensityAltitudePressureAltitude ::
  (
    Floating a
  , HasIndicatedAltitude s a
  , HasQNH s a
  , HasTemperature s a
  ) =>
  s
  -> DensityAltitudePressureAltitude a a


calculateTrueAirspeedDensityAltitude ::
  (
    Floating a
  , HasPressureAltitude s a
  , HasTemperature s a
  , HasIndicatedAirspeed s a
  ) =>
  s
  -> TrueAirspeedDensityAltitude a a  
-}
