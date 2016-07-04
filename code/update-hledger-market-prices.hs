#!/usr/bin/env stack
-- stack --resolver nightly-2016-07-03 --install-ghc runghc --package classy-prelude --package ConfigFile --package shakespeare --package yahoo-finance-api -- -Wall

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ClassyPrelude

import Control.Monad.Except ( runExceptT )
import Data.ConfigFile
    ( ConfigParser, CPError, CPErrorData, emptyCP, get, readfile, to_string )
import Data.Text ( replace )
import System.Directory ( getHomeDirectory, listDirectory )
import System.Exit ( exitFailure )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Shakespeare.Text ( st )
import Web.Yahoo.Finance ( getQuote )

configFilePath :: FilePath
configFilePath = unpack configFilePathText

-- | Full path to the config rc file.
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

-- | Die with the error message.
die :: forall a . Text -> IO a
die msg = do
    putStrLn $ "ERROR! " <> msg
    putStrLn dieMsg
    exitFailure

-- | Get the market prices directory from the config file.
getMarketPricesDir :: IO FilePath
getMarketPricesDir = do
    (eitherConfigParser :: Either CPError ConfigParser) <-
        handle couldNotReadConfFileHandler $ readfile emptyCP configFilePath
    configParser <- either noParseError pure eitherConfigParser
    -- putStrLn $ pack $ to_string configParser
    let eitherMarketPricesDir = get configParser "DEFAULT" "market-prices-dir"
    either noMarketPricesDir pure eitherMarketPricesDir
  where
    couldNotReadConfFileHandler :: forall a . SomeException -> IO a
    couldNotReadConfFileHandler _ = die $ "Could not read config file at \""
        <> configFilePathText <> "\"."

    noParseError :: forall a . (CPErrorData, String) -> IO a
    noParseError (cpErrorData, string) = die $
        "Could not parse config file at \"" <> configFilePathText <> "\".\n"
            <> tshow cpErrorData <> "\n" <> "Location: " <> pack string

    noMarketPricesDir :: forall a . (CPErrorData, String) -> IO a
    noMarketPricesDir _ = die $
        "Cannot read \"market-prices-dir\" option in config file at \""
            <> configFilePathText <> "\"."

-- | Replace all occurances of @~@ in the input 'Text' with the user's current
-- HOME directory.
replaceTildeWithHomeDir :: Text -> Text
replaceTildeWithHomeDir = replace "~" (pack $ unsafePerformIO getHomeDirectory)

replaceTildeWithHomeDirString :: String -> String
replaceTildeWithHomeDirString =
    unpack . replaceTildeWithHomeDir . pack

-- | Get all the market price files from the market price file directory.
getMarketPriceFiles :: FilePath -> IO [FilePath]
getMarketPriceFiles marketPricesDir =
    filter (isSuffixOf ".ledger") <$> listDirectory marketPricesDir

main :: IO ()
main = do
    marketPricesDir <- replaceTildeWithHomeDirString <$> getMarketPricesDir
    marketPriceFiles <- getMarketPriceFiles marketPricesDir
    print marketPriceFiles

