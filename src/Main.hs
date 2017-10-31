{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.E6B.CLI()
import Data.Aviation.E6B(parserE6BOptions, parseDouble)
import Options.Applicative(fullDesc, progDesc, header, execParser, info, helper)
import Papa

main ::
  IO ()
main =
  let im = fullDesc <> progDesc "e6b flight computer functions for aviation" <> header "e6b flight computer"
  in  do  q <- execParser (info (parserE6BOptions parseDouble parseDouble parseDouble parseDouble parseDouble <**> helper) im)
          putStrLn (show q)
