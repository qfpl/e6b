{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Applicative
import Data.Aviation.E6B
import Data.Foldable
import Options.Applicative
import Text.Parsec(Parsec, SourceName, parse)
import Text.Parser.Combinators(try, optional)
import Text.Parser.Char(CharParsing, spaces, char)
import Text.Parser.Token(TokenParsing, naturalOrDouble)
import Papa hiding (option)

main ::
  IO ()
main =
  let im = fullDesc <> progDesc "e6b flight computer functions for aviation" <> header "e6b flight computer"
  in  do  q <- execParser (info (parserE6BOptions <**> helper) im)
          putStrLn (show q)

data E6BOptions ia qnh tmp ias pa tmp' =
  IsIndicatedAltitudeQNHTemperature (IndicatedAltitudeQNHTemperature ia qnh tmp)
  | IsIndicatedAirspeedPressureAltitudeTemperature (IndicatedAirspeedPressureAltitudeTemperature ias pa tmp')
  deriving (Eq, Ord, Show)

parserE6BOptions' ::
  Parsec String () ia
  -> Parsec String () qnh
  -> Parsec String () tmp
  -> Parsec String () ias
  -> Parsec String () pa
  -> Parsec String () tmp'
  -> Parser (E6BOptions ia qnh tmp ias pa tmp')
parserE6BOptions' iaP qnhP tmpP iasP paP tmp'P =
  IsIndicatedAltitudeQNHTemperature <$> parserIndicatedAltitudeQNHTemperature' iaP qnhP tmpP <|>
  IsIndicatedAirspeedPressureAltitudeTemperature <$> parserIndicatedAirspeedPressureAltitudeTemperature' iasP paP tmp'P

parserE6BOptions ::
  Parser (E6BOptions Double Double Double Double Double Double)
parserE6BOptions =
  parserE6BOptions' parseDouble parseDouble parseDouble parseDouble parseDouble parseDouble

parserIndicatedAltitudeQNHTemperature' ::
  Parsec String () ia
  -> Parsec String () qnh
  -> Parsec String () tmp
  -> Parser (IndicatedAltitudeQNHTemperature ia qnh tmp)
parserIndicatedAltitudeQNHTemperature' iaP qnhP tmpP =
  IndicatedAltitudeQNHTemperature <$>
  parserIndicatedAltitude iaP <*>
  parserQNH qnhP <*>
  parserTemperature tmpP

parserIndicatedAltitudeQNHTemperature ::
  Parser (IndicatedAltitudeQNHTemperature Double Double Double)
parserIndicatedAltitudeQNHTemperature =
  parserIndicatedAltitudeQNHTemperature' parseDouble parseDouble parseDouble

parserIndicatedAirspeedPressureAltitudeTemperature' ::
  Parsec String () ias
  -> Parsec String () pa
  -> Parsec String () tmp
  -> Parser (IndicatedAirspeedPressureAltitudeTemperature ias pa tmp)
parserIndicatedAirspeedPressureAltitudeTemperature' iasP paP tmpP =
  IndicatedAirspeedPressureAltitudeTemperature <$>
  parserIndicatedAirspeed iasP <*>
  parserPressureAltitude paP <*>
  parserTemperature tmpP

parserIndicatedAirspeedPressureAltitudeTemperature ::
  Parser (IndicatedAirspeedPressureAltitudeTemperature Double Double Double)
parserIndicatedAirspeedPressureAltitudeTemperature =
  parserIndicatedAirspeedPressureAltitudeTemperature' parseDouble parseDouble parseDouble

----

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

readDensityAltitude' ::
  SourceName
  -> Parsec String () a
  -> ReadM (DensityAltitude a)
readDensityAltitude' x =
  parseRead x . parseDensityAltitude'
  
readDensityAltitude ::
  SourceName
  -> ReadM (DensityAltitude Double)
readDensityAltitude x =
  readDensityAltitude' x parseDouble

parserDensityAltitude ::
  Parsec String () a
  -> Parser (DensityAltitude a)
parserDensityAltitude daP =
  option (readDensityAltitude' "idensity altitude parser" daP) (long "density-altitude" <> short 'a' <> help ("density altitude, a numeric value and unit of measurement of distance. " ++ printMeasures measuresDistanceUnit))

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

readIndicatedAirspeed' ::
  SourceName
  -> Parsec String () a
  -> ReadM (IndicatedAirspeed a)
readIndicatedAirspeed' x =
  parseRead x . parseIndicatedAirspeed'
  
readIndicatedAirspeed ::
  SourceName
  -> ReadM (IndicatedAirspeed Double)
readIndicatedAirspeed x =
  readIndicatedAirspeed' x parseDouble

parserIndicatedAirspeed ::
  Parsec String () a
  -> Parser (IndicatedAirspeed a)
parserIndicatedAirspeed iasP =
  option (readIndicatedAirspeed' "indicated airspeed parser" iasP) (long "indicated-airspeed" <> short 's' <> help ("indicated airspeed, a numeric value and unit of measurement of velocity. " ++ printMeasures measuresVelocityUnit))

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

readIndicatedAltitude' ::
  SourceName
  -> Parsec String () a
  -> ReadM (IndicatedAltitude a)
readIndicatedAltitude' x =
  parseRead x . parseIndicatedAltitude'
  
readIndicatedAltitude ::
  SourceName
  -> ReadM (IndicatedAltitude Double)
readIndicatedAltitude x =
  readIndicatedAltitude' x parseDouble

parserIndicatedAltitude ::
  Parsec String () a
  -> Parser (IndicatedAltitude a)
parserIndicatedAltitude iaP =
  option (readIndicatedAltitude' "indicated altitude parser" iaP) (long "indicated-altitude" <> short 'a' <> help ("indicated altitude, a numeric value and unit of measurement of distance. " ++ printMeasures measuresDistanceUnit))

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

readPressureAltitude' ::
  SourceName
  -> Parsec String () a
  -> ReadM (PressureAltitude a)
readPressureAltitude' x =
  parseRead x . parsePressureAltitude'
  
readPressureAltitude ::
  SourceName
  -> ReadM (PressureAltitude Double)
readPressureAltitude x =
  readPressureAltitude' x parseDouble

parserPressureAltitude ::
  Parsec String () a
  -> Parser (PressureAltitude a)
parserPressureAltitude paP =
  option (readPressureAltitude' "pressure altitude parser" paP) (long "pressure-altitude" <> short 'a' <> help ("pressure altitude, a numeric value and unit of measurement of distance. " ++ printMeasures measuresDistanceUnit))

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

readQNH' ::
  SourceName
  -> Parsec String () a
  -> ReadM (QNH a)
readQNH' x =
  parseRead x . parseQNH'
  
readQNH ::
  SourceName
  -> ReadM (QNH Double)
readQNH x =
  readQNH' x parseDouble

parserQNH ::
  Parsec String () a
  -> Parser (QNH a)
parserQNH qnhP =
  option (readQNH' "QNH parser" qnhP) (long "qnh" <> short 'q' <> help ("QNH, a numeric value and unit of measurement of pressure. " ++ printMeasures measuresPressureUnit))

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

readTemperature' ::
  SourceName
  -> Parsec String () a
  -> ReadM (Temperature a)
readTemperature' x =
  parseRead x . parseTemperature'
  
readTemperature ::
  SourceName
  -> ReadM (Temperature Double)
readTemperature x =
  readTemperature' x parseDouble

parserTemperature ::
  Parsec String () a
  -> Parser (Temperature a)
parserTemperature tmpP =
  option (readTemperature' "temperature parser" tmpP) (long "temperature" <> short 't' <> help ("temperature, a numeric value and unit of measurement of temperature. " ++ printMeasures measuresTemperatureUnit))

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

readTrueAirspeed' ::
  SourceName
  -> Parsec String () a
  -> ReadM (TrueAirspeed a)
readTrueAirspeed' x =
  parseRead x . parseTrueAirspeed'
  
readTrueAirspeed ::
  SourceName
  -> ReadM (TrueAirspeed Double)
readTrueAirspeed x =
  readTrueAirspeed' x parseDouble

parserTrueAirspeed ::
  Parsec String () a
  -> Parser (TrueAirspeed a)
parserTrueAirspeed tmpP =
  option (readTrueAirspeed' "true airspeed parser" tmpP) (long "true-airspeed" <> short 's' <> help ("true airspeed, a numeric value and unit of measurement of velocity. " ++ printMeasures measuresVelocityUnit))

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
