#!/usr/bin/env stack
-- stack --resolver nightly-2016-07-03 --install-ghc runghc --package classy-prelude --package ConfigFile --package shakespeare --package yahoo-finance-api -- -Wall

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude

import Control.Monad.Except ( runExceptT )
import Data.ConfigFile ( ConfigParser, CPError, CPErrorData, emptyCP, readfile, to_string )
import Data.Text ( replace )
import System.Directory ( getHomeDirectory )
import System.Exit ( exitFailure )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Shakespeare.Text ( st )
import Web.Yahoo.Finance ( getQuote )

configFilePath :: FilePath
configFilePath = unpack configFilePathText

configFilePathText :: Text
configFilePathText = replaceTildeWithHomeDir "~/.update-hledger-market-prices.rc"

dieMsg :: Text
dieMsg =
    [st|
You must have a configuration file at "~/.update-hledger-market-prices.rc"
that looks like the following:

```
market-prices-dir = ~/some/path/to/market-prices
``` |]

die :: forall a . Text -> IO a
die msg = do
    putStrLn $ "ERROR! " <> msg
    putStrLn dieMsg
    exitFailure

getMarketPricesDir :: IO FilePath
getMarketPricesDir = do
    (val :: Either CPError ConfigParser) <-
        handle couldNotReadConfFileHandler $ readfile emptyCP configFilePath
    configParser <- either noParseError pure val
    putStrLn $ pack $ to_string configParser
    -- get configParser "DEFAULT" 
    undefined
  where
    couldNotReadConfFileHandler :: forall a . SomeException -> IO a
    couldNotReadConfFileHandler _ = do
        let couldNotReadConfFileMsg = "Could not read config file at \""
                <> configFilePathText <> "\"."
        die couldNotReadConfFileMsg

    noParseError :: forall a . (CPErrorData, String) -> IO a
    noParseError (cpErrorData, string) = do
        let couldNotParseConfFileMsg = "Could not parse config file at \""
                <> configFilePathText <> "\".\n"
                <> tshow cpErrorData <> "\n"
                <> "Location: " <> pack string
        die couldNotParseConfFileMsg

replaceTildeWithHomeDir :: Text -> Text
replaceTildeWithHomeDir = replace "~" (pack $ unsafePerformIO getHomeDirectory)

main :: IO ()
main = do
    getMarketPricesDir
    undefined

