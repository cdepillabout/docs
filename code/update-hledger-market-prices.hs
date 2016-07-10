#!/usr/bin/env stack
-- stack --resolver nightly-2016-07-09 --install-ghc runghc --package classy-prelude --package ConfigFile --package filepath --package formatting --package http-client-tls --package servant-client --package shakespeare --package text --package yahoo-finance-api -- -Wall

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude

import Control.Monad.Except ( runExceptT )
import Data.ConfigFile
    ( ConfigParser, CPError, CPErrorData, emptyCP, get, readfile )
import Data.Text ( replace )
import Formatting ( format, fixed )
import Network.HTTP.Client.TLS ( getGlobalManager )
import Servant.Client ( ServantError )
import System.Directory ( getHomeDirectory, listDirectory )
import System.Exit ( exitFailure )
import System.FilePath ( dropExtensions, takeFileName )
import System.IO ( appendFile )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Shakespeare.Text ( st )
import Web.Yahoo.Finance ( Quote(..), QuoteList(..), StockSymbol(..), getQuote )

configFilePath :: FilePath
configFilePath = unpack configFilePathText

-- | Full path to the config rc file.
configFilePathText :: Text
configFilePathText = replaceTildeWithHomeDir "~/.update-hledger-market-prices.rc"

dieConfigMsg :: Text
dieConfigMsg =
    [st|
You must have a configuration file at "~/.update-hledger-market-prices.rc"
that looks like the following:

```
market-prices-dir = ~/some/path/to/market-prices
``` |]

-- | Die with the error message.
dieMsg :: forall a . Text -> IO a
dieMsg msg = do
    putStrLn $ "ERROR! " <> msg
    putStrLn dieConfigMsg
    exitFailure

-- | Die with the error message.
dieError :: forall a . Text -> IO a
dieError msg = do
    putStrLn $ "ERROR! " <> msg
    exitFailure

-- | Get the market prices directory from the config file.
getMarketPricesDir :: IO FilePath
getMarketPricesDir = do
    (eitherConfigParser :: Either CPError ConfigParser) <-
        handle couldNotReadConfFileHandler $ readfile emptyCP configFilePath
    configParser <- either noParseError pure eitherConfigParser
    let eitherMarketPricesDir = get configParser "DEFAULT" "market-prices-dir"
    either noMarketPricesDir pure eitherMarketPricesDir
  where
    couldNotReadConfFileHandler :: forall a . SomeException -> IO a
    couldNotReadConfFileHandler _ = dieMsg $ "Could not read config file at \""
        <> configFilePathText <> "\"."

    noParseError :: forall a . (CPErrorData, String) -> IO a
    noParseError (cpErrorData, string) = dieMsg $
        "Could not parse config file at \"" <> configFilePathText <> "\".\n"
            <> tshow cpErrorData <> "\n" <> "Location: " <> pack string

    noMarketPricesDir :: forall a . (CPErrorData, String) -> IO a
    noMarketPricesDir _ = dieMsg $
        "Cannot read \"market-prices-dir\" option in config file at \""
            <> configFilePathText <> "\"."

-- | Replace all occurances of @~@ in the input 'Text' with the user's current
-- HOME directory.
replaceTildeWithHomeDir :: Text -> Text
replaceTildeWithHomeDir = replace "~" (pack $ unsafePerformIO getHomeDirectory)

-- | Like 'replaceTildeWithHomeDir' but operating on 'String's.
replaceTildeWithHomeDirString :: String -> String
replaceTildeWithHomeDirString =
    unpack . replaceTildeWithHomeDir . pack

-- | Get all the market price files from the market price file directory.
getMarketPriceFiles :: FilePath -> IO [FilePath]
getMarketPriceFiles marketPricesDir = listDirectory marketPricesDir >>=
    pure . fmap ((marketPricesDir <> "/") <>) >>=
    pure . filter (isSuffixOf ".ledger")

-- | Take list of 'FilePath's, and filter out those 'FilePath's that have the
-- string @"NOT_YAHOO_FINANCE_MARKET_PRICE"@ in them.
filterOutNonYahooFiles :: [FilePath] -> IO [FilePath]
filterOutNonYahooFiles = filterM (isNonYahooFile >=> pure . not)

-- | Read the contents of a FilePath and check if the string
-- @"NOT_YAHOO_FINANCE_MARKET_PRICE"@ exists in the file.  If it does, then
-- return 'True'.
isNonYahooFile :: FilePath -> IO Bool
isNonYahooFile marketPriceFile =
    isInfixOf "NOT_YAHOO_FINANCE_MARKET_PRICE"
        <$> (readFile marketPriceFile :: IO Text)

updateMarketPricesForYahooFiles :: [FilePath] -> IO ()
updateMarketPricesForYahooFiles marketPriceFiles = do
    let stockSymbols = fmap getStockSymbolFromFilePath marketPriceFiles
        pathAndStockSymbol = zip marketPriceFiles stockSymbols
    manager <- getGlobalManager
    eitherMarketPriceData <-
        runExceptT $ runReaderT (getQuote stockSymbols) manager
    marketPriceData <-
        either couldNotGetPricesFromYahoo pure eitherMarketPriceData
    let maybeMatchedData =
            matchYahooDataToStockSymbol marketPriceData pathAndStockSymbol
    matchedData <-
        maybe
            (couldNotDetermineDataFromYahoo marketPriceData)
            pure
            maybeMatchedData
    traverse_ updateMarketPriceForYahooFile matchedData
  where
    couldNotGetPricesFromYahoo :: forall a . ServantError -> IO a
    couldNotGetPricesFromYahoo servantError =
        dieError $ "Couldn't get stock data from Yahoo:\n" <>
            tshow servantError

    couldNotDetermineDataFromYahoo :: forall a . QuoteList -> IO a
    couldNotDetermineDataFromYahoo quoteList =
        dieError $ "Couldn't match stock data from yahoo with what is on disk:\n" <>
            tshow quoteList

matchYahooDataToStockSymbol
    :: QuoteList -> [(FilePath, StockSymbol)] -> Maybe [(FilePath, Quote)]
matchYahooDataToStockSymbol quoteList =
    traverse (findQuoteForStockSymbol quoteList)

findQuoteForStockSymbol :: QuoteList -> (FilePath, StockSymbol) -> Maybe (FilePath, Quote)
findQuoteForStockSymbol (QuoteList quoteList) (path, stockSymbol) =
    (path,) <$> find (isQuoteForStockSymbol stockSymbol) quoteList

isQuoteForStockSymbol :: StockSymbol -> Quote -> Bool
isQuoteForStockSymbol (StockSymbol stockSymbol) Quote{..} =
    quoteSymbol == stockSymbol

getStockSymbolFromFilePath :: FilePath -> StockSymbol
getStockSymbolFromFilePath = StockSymbol . pack . dropExtensions . takeFileName

updateMarketPriceForYahooFile :: (FilePath, Quote) -> IO ()
updateMarketPriceForYahooFile (marketPriceFile, Quote{..}) = do
    let maybePrice = readPrice quotePrice
    price <- maybe couldNotReadPrice pure maybePrice
    let date = pack $ formatTime defaultTimeLocale "%Y/%m/%d" quoteUTCTime
    let marketPriceText = "P " <> date <> " " <> quoteSymbol <> " $" <> price <> "\n"
        marketPriceString = unpack marketPriceText
    appendFile marketPriceFile marketPriceString
  where
    couldNotReadPrice :: forall a . IO a
    couldNotReadPrice = dieError $
        "Could not read price \"" <> quotePrice <> "\" for file "
            <> pack marketPriceFile

readPrice :: Text -> Maybe Text
readPrice priceText = toStrict . format (fixed 2) <$> (readMay priceText :: Maybe Double)


main :: IO ()
main = do
    marketPricesDir <- replaceTildeWithHomeDirString <$> getMarketPricesDir
    marketPriceFiles <- getMarketPriceFiles marketPricesDir
    yahooMarketPriceFiles <- filterOutNonYahooFiles marketPriceFiles
    updateMarketPricesForYahooFiles yahooMarketPriceFiles
