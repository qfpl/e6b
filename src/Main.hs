{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Applicative
import Data.Aviation.E6B
import Data.Foldable
-- import Options.Applicative
import Text.Parser.Combinators(try, optional)
import Text.Parser.Char(CharParsing, string, spaces, char)
import Text.Parser.Token(TokenParsing, naturalOrDouble)
import Text.Parsec()
import Papa

main ::
  IO ()
main =
  putStrLn "e6b"

data ADistance a =
  ADistance a UnitOfDistance
  deriving (Eq, Show)

data UnitOfDistance = Metre' | Foot' | Inch'
  deriving (Eq, Show)

parseADistance ::
  (TokenParsing f, CharParsing f) =>
  f (ADistance Double)
parseADistance =
  ADistance <$> parseDouble <* spaces <*> parseUnitOfDistance

parseUnitOfDistance ::
  CharParsing f =>
  f UnitOfDistance
parseUnitOfDistance =
  let r = 
        [
          ("metre" , Metre')
        , ("metres", Metre')
        , ("meter" , Metre')
        , ("meters", Metre')
        , ("m"     , Metre')
        , ("foot"  , Foot')
        , ("feet"  , Foot')
        , ("ft"    , Foot')
        , ("inch"  , Inch')
        , ("inches", Inch')
        , ("in"    , Inch')
        ]
  in  asum
        ((\(s, x) -> x <$ try (string s)) <$> r)

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
