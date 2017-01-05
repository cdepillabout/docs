#!/usr/bin/env stack
-- stack --verbose --resolver lts-7.14 --install-ghc --no-system-ghc runghc --package classy-prelude --package hledger-lib --package hledger --package pretty-simple --package from-sum --package optparse-applicative

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude

import Control.FromSum (fromEitherOrM, fromMaybeOrM)
import Data.Data (Data)
import Hledger.Cli.Add (transactionsSimilarTo)
import Hledger.Cli.CliOptions (CliOpts(..))
import Hledger.Cli.Main (argsToCliOpts)
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.Types (Journal, Transaction)
import Hledger.Reports.ReportOptions (ReportOpts, queryFromOptsOnly)
import Options.Applicative
       (Parser, ParserInfo, argument, execParser, fullDesc, header,
        helper, info, metavar, progDesc, str)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Parsec
       (Parsec, ParseError, (<?>), char, digit, getInput, many1, noneOf,
        optionMaybe, parse, parserFail, spaces)
import qualified Text.Parsec as Parsec
import Text.Pretty.Simple (pPrint)

data SimpleEntry = SimpleEntry
  { amount :: PositiveInt
  , description :: Text
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

data SimpleEntriesOnDate = SimpleEntriesOnDate
  { day :: Day
  , simpleEntries :: [SimpleEntry]
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

data DatedEntry = DatedEntry
  { day :: Day
  , amount :: PositiveInt
  , description :: Text
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

simpleEntryToDatedEntry :: Day -> SimpleEntry -> DatedEntry
simpleEntryToDatedEntry day SimpleEntry {amount, description} =
  DatedEntry day amount description

simpleEntriesOnDateToDatedEntries :: SimpleEntriesOnDate -> [DatedEntry]
simpleEntriesOnDateToDatedEntries SimpleEntriesOnDate {day, simpleEntries} =
  simpleEntryToDatedEntry day <$> simpleEntries

die :: MonadIO m => Text -> m a
die msg = do
  putStrLn $ "ERROR: " <> msg
  liftIO exitFailure

getHledgerCliOptsWithJournal :: MonadIO m => m CliOpts
getHledgerCliOptsWithJournal = do
  maybeDefaultLedger <- liftIO $ lookupEnv "HLEDGER_DEFAULT_LEDGER"
  defaultLedger <- fromMaybeOrM maybeDefaultLedger $
    die "Please define the \"HLEDGER_DEFAULT_LEDGER\" environment variable."
  cliOpts <- liftIO $ argsToCliOpts [] []
  let cliOptsWithJournal = cliOpts { file_ = [defaultLedger] }
  return cliOptsWithJournal

main :: IO ()
main = do
  hledgerCliOpts <- getHledgerCliOptsWithJournal
  options <- parseCmdLineOptions
  datedInputEntries <- readDatedEntries options
  withJournalDo hledgerCliOpts (what datedInputEntries)

similarTransaction :: Day -> ReportOpts -> Journal -> Text -> Maybe Transaction
similarTransaction day reportOpts journal description =
  let query = queryFromOptsOnly day reportOpts
      historyMatches = transactionsSimilarTo journal query description
      bestMatch = listToMaybe historyMatches
  in fmap snd bestMatch

getSimilarTransaction :: Day -> CliOpts -> Journal -> Text -> Maybe Transaction
getSimilarTransaction today hledgerCliOpts journal desc =
  let reportOpts = reportopts_ hledgerCliOpts
      maybeTrans = similarTransaction today reportOpts journal "breakfast"
  in maybeTrans

what :: MonadIO m => [DatedEntry] -> CliOpts -> Journal -> m ()
what inputEntries hledgerCliOpts journal = do
  pPrint inputEntries
  undefined

--------------------------------
-- Parse Command Line Options --
--------------------------------

data Options = Options
  { inputFilePath :: FilePath
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

optionsParser :: Parser Options
optionsParser = Options <$> argument str (metavar "INPUT_FILE")

parseCmdLineOptions :: MonadIO m => m Options
parseCmdLineOptions = liftIO $ execParser opts
  where
    opts :: ParserInfo Options
    opts =
      info
        (helper <*> optionsParser)
        (fullDesc `mappend`
         progDesc
           "Turn monthly expenses lists from Google Keep into hledger format." `mappend`
         header "update-hledger-monthly - input hledger monthly files")

----------------------
-- Parse Input File --
----------------------

type MyParser = Parsec Text ()

readInputFile :: MonadIO m => Options -> m Text
readInputFile = readFile . inputFilePath

readDatedEntries :: MonadIO m => Options -> m [DatedEntry]
readDatedEntries options = do
  today <- liftIO getCurrentDay
  inputFile <- readInputFile options
  let eitherSimpleEntriesOnDate = parseInputFile today options inputFile
  simpleEntriesOnDate <- fromEitherOrM eitherSimpleEntriesOnDate $ \parseError ->
      die $ "Got parser error: " <> tshow parseError
  pure $ concatMap simpleEntriesOnDateToDatedEntries simpleEntriesOnDate

parseInputFile :: Day -> Options -> Text -> Either ParseError [SimpleEntriesOnDate]
parseInputFile today options inputFile =
  parse (simpleEntriesOnDatesParser today) (inputFilePath options) inputFile

simpleEntriesOnDatesParser :: Day -> MyParser [SimpleEntriesOnDate]
simpleEntriesOnDatesParser today = spaces *> many1 (simpleEntriesOnDateParser today)

simpleEntriesOnDateParser :: Day -> MyParser SimpleEntriesOnDate
simpleEntriesOnDateParser today = do
  day <- dayParser today
  spaces
  simpleEntries <- many1 $ simpleEntryParser today
  let simpleEntriesOnDate = SimpleEntriesOnDate day simpleEntries
  pure simpleEntriesOnDate

-- | First, try to parse a Day.  If it succeeds, then the next thing to parse
-- is a 'SimpleEntriesOnDate' with 'simpleEntriesOnDateParser' (which begins
-- with a 'Day'), so make this parser fail without consuming any input.
--
-- If it doesn't succeed, then parse a 'SimpleEntry'.
simpleEntryParser :: Day -> MyParser SimpleEntry
simpleEntryParser today = do
  restOfInput <- getInput
  let maybeDay = eitherToMaybe $ parse (dayParser today) "" restOfInput
  maybe
    simpleEntryParser'
    (const $ parserFail "able to parse a day, so failing the simpleEntryParser.")
    maybeDay
  where
    simpleEntryParser' :: MyParser SimpleEntry
    simpleEntryParser' = do
      positiveInt <- positiveIntParser
      spaces
      desc <- many1 $ noneOf "\n\r"
      spaces
      let simpleEntry = SimpleEntry positiveInt $ pack desc
      pure simpleEntry

dayParser :: Day -> MyParser Day
dayParser today = do
  PositiveInt month <- positiveIntParser
  char '/'
  PositiveInt day <- positiveIntParser
  let year = calcYear today month
      dayString = show year <> "-" <> pad month <> "-" <> pad day
      maybeDay = parseTimeM True defaultTimeLocale "%F" dayString
  fromMaybeOrM maybeDay $
    parserFail $ "Could not parse \"" <> dayString <> "\" as a Day."
  where
    pad :: Int -> String
    pad int =
      if int < 10
        then "0" <> show int
        else show int

-- | calculate the year to use for an input month.  If it is currently January
-- or February, and the input month is November or December, then use the
-- previous year.  Otherwise, use the current year.
calcYear :: Day -> Int -> Integer
calcYear today month =
  let (yearForToday, monthForToday, _) = toGregorian today
  in if (monthForToday == 1 || monthForToday == 2) && (month == 11 || month == 12)
       then yearForToday - 1
       else yearForToday

positiveIntParser :: MyParser PositiveInt
positiveIntParser = do
  digitsText <- pack <$> many1 digit <?> "Trying to parse a set of digits."
  fromEitherOrM (positiveInt digitsText) $ parserFail . unpack

----------------------
-- Helper functions --
----------------------

-- | Convert an 'Either' to a 'Maybe'.
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

-- | Newtype wrapper for 'Int's that are above 0.
newtype PositiveInt = PositiveInt
  { unPositiveIntText :: Int
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | Read in a 'PositiveInt' from a 'Text'.
positiveInt :: Text -> Either Text PositiveInt
positiveInt inputText =
  let maybeInt = readMay inputText
  in case maybeInt of
       Nothing ->
         Left $ "Cannot read input \"" <> inputText <> "\" as a positive int."
       Just int ->
         if int > 0
           then pure $ PositiveInt int
           else Left $
                "input \"" <> inputText <>
                "\" is less than 0. Not a positive int."
