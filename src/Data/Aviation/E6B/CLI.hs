{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.E6B.CLI(
  E6BOptions(..)
, parserE6BOptions
, parseDensityAltitude
, readDensityAltitude
, parserDensityAltitude
, parseIndicatedAirspeed
, readIndicatedAirspeed
, parserIndicatedAirspeed
, parseIndicatedAltitude
, readIndicatedAltitude
, parserIndicatedAltitude
, parsePressureAltitude
, readPressureAltitude
, parserPressureAltitude
, parseQNH
, readQNH
, parserQNH
, parseTemperature
, readTemperature
, parserTemperature
, parseTrueAirspeed
, readTrueAirspeed
, parserTrueAirspeed
, measuresDistanceUnit
, parseDistanceUnit
, measuresPressureUnit
, parsePressureUnit
, measuresTemperatureUnit
, parseTemperatureUnit
, measuresVelocityUnit
, parseVelocityUnit
, parseUnit
, parseDouble
, parseRead
, caseInsensitiveString
, printMeasures
) where

import Data.Aviation.E6B.IndicatedAltitudeQNHTemperature(IndicatedAltitudeQNHTemperature(IndicatedAltitudeQNHTemperature))
import Data.Aviation.E6B.IndicatedAirspeedPressureAltitudeTemperature(IndicatedAirspeedPressureAltitudeTemperature(IndicatedAirspeedPressureAltitudeTemperature))
import Data.Aviation.E6B.DensityAltitude(DensityAltitude(DensityAltitude))
import Data.Aviation.E6B.IndicatedAirspeed(IndicatedAirspeed(IndicatedAirspeed))
import Data.Aviation.E6B.IndicatedAltitude(IndicatedAltitude(IndicatedAltitude))
import Data.Aviation.E6B.PressureAltitude(PressureAltitude(PressureAltitude))
import Data.Aviation.E6B.QNH(QNH(QNH))
import Data.Aviation.E6B.Temperature(Temperature(Temperature))
import Data.Aviation.E6B.TrueAirspeed(TrueAirspeed(TrueAirspeed))
import Data.Aviation.E6B.DistanceUnit(DistanceUnit(Kilometre, Centimetre, Micrometre, StatuteMile, Yard, Foot, Inch, NauticalMile, Millimetre, Metre))
import Data.Aviation.E6B.PressureUnit(PressureUnit(Pascal, Hectopascal, InHg, Psi, Torr, Atmosphere, Bar))
import Data.Aviation.E6B.TemperatureUnit(TemperatureUnit(Celsius, Fahrenheit, Kelvin))
import Data.Aviation.E6B.VelocityUnit(VelocityUnit(Knot, KilometreHour, StatuteMileHour, MetreSecond, FootSecond))
import Options.Applicative(Parser, ReadM, option, long, short, help, eitherReader)
import Text.Parsec(Parsec, SourceName, parse)
import Text.Parser.Combinators(try, optional)
import Text.Parser.Char(CharParsing, spaces, char)
import Text.Parser.Token(TokenParsing, naturalOrDouble)
import Papa hiding (option)

data E6BOptions ia qnh tmp ias pa =
  IsIndicatedAltitudeQNHTemperature (IndicatedAltitudeQNHTemperature ia qnh tmp)
  | IsIndicatedAirspeedPressureAltitudeTemperature (IndicatedAirspeedPressureAltitudeTemperature ias pa tmp)
  deriving (Eq, Ord, Show)

parserE6BOptions ::
  Parsec String () ia
  -> Parsec String () qnh
  -> Parsec String () tmp
  -> Parsec String () ias
  -> Parsec String () pa 
  -> Parser (E6BOptions ia qnh tmp ias pa)
parserE6BOptions iaP qnhP tmpP iasP paP =
  (\tmp e ->  case e of 
                Left (ia, qnh) ->
                  IsIndicatedAltitudeQNHTemperature (IndicatedAltitudeQNHTemperature ia qnh tmp)
                Right (ias, pa) ->
                  IsIndicatedAirspeedPressureAltitudeTemperature (IndicatedAirspeedPressureAltitudeTemperature ias pa tmp)) <$> parserTemperature tmpP <*> (Left <$> ((,) <$> parserIndicatedAltitude iaP <*> parserQNH qnhP) <|> Right <$> ((,) <$> parserIndicatedAirspeed iasP <*> parserPressureAltitude paP))

----

parseDensityAltitude ::
  CharParsing f =>
  f a
  -> f (DensityAltitude a)
parseDensityAltitude p =
  DensityAltitude <$> p <* spaces <*> parseDistanceUnit

readDensityAltitude ::
  SourceName
  -> Parsec String () a
  -> ReadM (DensityAltitude a)
readDensityAltitude x =
  parseRead x . parseDensityAltitude
  
parserDensityAltitude ::
  Parsec String () a
  -> Parser (DensityAltitude a)
parserDensityAltitude daP =
  option (readDensityAltitude "density altitude parser" daP) (long "density-altitude" <> short 'a' <> help ("density altitude, a numeric value and unit of measurement of distance. " ++ printMeasures measuresDistanceUnit))

parseIndicatedAirspeed ::
  CharParsing f =>
  f a
  -> f (IndicatedAirspeed a)
parseIndicatedAirspeed p =
  IndicatedAirspeed <$> p <* spaces <*> parseVelocityUnit

readIndicatedAirspeed ::
  SourceName
  -> Parsec String () a
  -> ReadM (IndicatedAirspeed a)
readIndicatedAirspeed x =
  parseRead x . parseIndicatedAirspeed
  
parserIndicatedAirspeed ::
  Parsec String () a
  -> Parser (IndicatedAirspeed a)
parserIndicatedAirspeed iasP =
  option (readIndicatedAirspeed "indicated airspeed parser" iasP) (long "indicated-airspeed" <> short 's' <> help ("indicated airspeed, a numeric value and unit of measurement of velocity. " ++ printMeasures measuresVelocityUnit))

parseIndicatedAltitude ::
  CharParsing f =>
  f a
  -> f (IndicatedAltitude a)
parseIndicatedAltitude p =
  IndicatedAltitude <$> p <* spaces <*> parseDistanceUnit

readIndicatedAltitude ::
  SourceName
  -> Parsec String () a
  -> ReadM (IndicatedAltitude a)
readIndicatedAltitude x =
  parseRead x . parseIndicatedAltitude
  
parserIndicatedAltitude ::
  Parsec String () a
  -> Parser (IndicatedAltitude a)
parserIndicatedAltitude iaP =
  option (readIndicatedAltitude "indicated altitude parser" iaP) (long "indicated-altitude" <> short 'a' <> help ("indicated altitude, a numeric value and unit of measurement of distance. " ++ printMeasures measuresDistanceUnit))

parsePressureAltitude ::
  CharParsing f =>
  f a
  -> f (PressureAltitude a)
parsePressureAltitude p =
  PressureAltitude <$> p <* spaces <*> parseDistanceUnit

readPressureAltitude ::
  SourceName
  -> Parsec String () a
  -> ReadM (PressureAltitude a)
readPressureAltitude x =
  parseRead x . parsePressureAltitude
  
parserPressureAltitude ::
  Parsec String () a
  -> Parser (PressureAltitude a)
parserPressureAltitude paP =
  option (readPressureAltitude "pressure altitude parser" paP) (long "pressure-altitude" <> short 'a' <> help ("pressure altitude, a numeric value and unit of measurement of distance. " ++ printMeasures measuresDistanceUnit))

parseQNH ::
  CharParsing f =>
  f a
  -> f (QNH a)
parseQNH p =
  QNH <$> p <* spaces <*> parsePressureUnit

readQNH ::
  SourceName
  -> Parsec String () a
  -> ReadM (QNH a)
readQNH x =
  parseRead x . parseQNH
  
parserQNH ::
  Parsec String () a
  -> Parser (QNH a)
parserQNH qnhP =
  option (readQNH "QNH parser" qnhP) (long "qnh" <> short 'q' <> help ("QNH, a numeric value and unit of measurement of pressure. " ++ printMeasures measuresPressureUnit))

parseTemperature ::
  CharParsing f =>
  f a
  -> f (Temperature a)
parseTemperature p =
  Temperature <$> p <* spaces <*> parseTemperatureUnit

readTemperature ::
  SourceName
  -> Parsec String () a
  -> ReadM (Temperature a)
readTemperature x =
  parseRead x . parseTemperature
  
parserTemperature ::
  Parsec String () a
  -> Parser (Temperature a)
parserTemperature tmpP =
  option (readTemperature "temperature parser" tmpP) (long "temperature" <> short 't' <> help ("temperature, a numeric value and unit of measurement of temperature. " ++ printMeasures measuresTemperatureUnit))

parseTrueAirspeed ::
  CharParsing f =>
  f a
  -> f (TrueAirspeed a)
parseTrueAirspeed p =
  TrueAirspeed <$> p <* spaces <*> parseVelocityUnit

readTrueAirspeed ::
  SourceName
  -> Parsec String () a
  -> ReadM (TrueAirspeed a)
readTrueAirspeed x =
  parseRead x . parseTrueAirspeed
  
parserTrueAirspeed ::
  Parsec String () a
  -> Parser (TrueAirspeed a)
parserTrueAirspeed tmpP =
  option (readTrueAirspeed "true airspeed parser" tmpP) (long "true-airspeed" <> short 's' <> help ("true airspeed, a numeric value and unit of measurement of velocity. " ++ printMeasures measuresVelocityUnit))

measuresDistanceUnit ::
  [(DistanceUnit, [String])]
measuresDistanceUnit =
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

parseDistanceUnit ::
  CharParsing f =>
  f DistanceUnit
parseDistanceUnit =
  parseUnit measuresDistanceUnit

measuresPressureUnit ::
  [(PressureUnit, [String])]
measuresPressureUnit =
  [
    (Pascal     , ["pascals", "pascal", "pa"])
  , (Hectopascal, ["hectopascals", "hectopascal", "hpa", "millibars", "millibar", "mb", "mbar"])
  , (InHg       , ["inhg", "\"hg", "”hg"])
  , (Psi        , ["psi", "lbf/in²", "lbf/in2", "lb/in", "lbf-in²", "lbf-in2", "lb-in"])
  , (Torr       , ["torrs", "torr"])
  , (Atmosphere , ["atmospheres", "atmosphere", "atm"])
  , (Bar        , ["bar"])
  ]

parsePressureUnit ::
  CharParsing f =>
  f PressureUnit
parsePressureUnit =
  parseUnit measuresPressureUnit

measuresTemperatureUnit ::
  [(TemperatureUnit, [String])]
measuresTemperatureUnit =
  [
    (Celsius   , ["celsius", "cels", "c", "°c"])
  , (Fahrenheit, ["fahrenheit", "fah", "fahr", "f", "°f"])
  , (Kelvin    , ["kelvin", "k", "°k"])
  ]

parseTemperatureUnit ::
  CharParsing f =>
  f TemperatureUnit
parseTemperatureUnit =
  parseUnit measuresTemperatureUnit

measuresVelocityUnit ::
  [(VelocityUnit, [String])]
measuresVelocityUnit =
  [
    (Knot   , ["knots", "knot", "kt", "kts", "kn"])
  , (KilometreHour, ["kph", "km/hr", "kmh", "kp/h", "km/h", "kmph", "k.p.h.", "km/hour"])
  , (StatuteMileHour    , ["mph", "mi/h"])
  , (MetreSecond    , ["m/s", "mps", "m·s⁻¹", "m s⁻¹"])
  , (FootSecond    , ["ft/s", "ft/sec", "fps", "ft s⁻¹"])
  ]

parseVelocityUnit ::
  CharParsing f =>
  f VelocityUnit
parseVelocityUnit =
  parseUnit measuresVelocityUnit

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

parseRead ::
  SourceName
  -> Parsec String () a
  -> ReadM a
parseRead x p =
  eitherReader (\s -> parse p x s & _Left %~ show)

caseInsensitiveString ::
  (CharParsing f, Traversable t) =>
  t Char
  -> f (t Char)
caseInsensitiveString s =
  let caseInsensitiveChar c =
        char (toLower c) <|> char (toUpper c)
  in  try (mapM caseInsensitiveChar s)

printMeasures ::
  [(a, [String])]
  -> String
printMeasures x =
  "[ " ++ (x >>= \(_, ss) -> ss >>= \s -> s ++ " ") ++ "]"
