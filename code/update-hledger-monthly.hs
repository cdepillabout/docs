#!/usr/bin/env stack
-- stack --verbose --resolver lts-7.14 --install-ghc --no-system-ghc runghc --package classy-prelude --package hledger-lib --package hledger --package pretty-simple --package from-sum --package optparse-applicative -- -Wall -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import ClassyPrelude

import Control.FromSum (fromEitherOrM, fromMaybeOrM)
import Data.Data (Data)
import Hledger.Cli.Add
       (journalAddTransaction, transactionsSimilarTo)
import Hledger.Cli.CliOptions (CliOpts(..))
import Hledger.Cli.Main (argsToCliOpts)
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.Amount (num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types
       (AccountName, Amount, Journal, MixedAmount(Mixed), Posting(..),
        Quantity, Transaction(..))
import Hledger.Reports.ReportOptions (queryFromOptsOnly)
import Options.Applicative
       (Parser, ParserInfo, argument, execParser, fullDesc, header,
        helper, info, metavar, progDesc, str)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Parsec
       (Parsec, ParseError, (<?>), char, digit, getInput, many1, noneOf,
        parse, parserFail, spaces)

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

getSimilarTransaction :: Day -> CliOpts -> Journal -> Text -> Maybe Transaction
getSimilarTransaction today hledgerCliOpts journal desc =
  let reportOpts = reportopts_ hledgerCliOpts
      query = queryFromOptsOnly today reportOpts
      historyMatches = transactionsSimilarTo journal query desc
      bestMatch = listToMaybe historyMatches
  in fmap snd bestMatch

what :: MonadIO m => [DatedEntry] -> CliOpts -> Journal -> m ()
what datedEntries hledgerCliOpts journal = do
  today <- liftIO getCurrentDay
  let transactions =
        snd <$>
        datedEntriesToTransactions
          today
          hledgerCliOpts
          journal
          datedEntries
  traverse_ (liftIO . journalAddTransaction journal hledgerCliOpts) transactions

datedEntriesToTransactions :: Day -> CliOpts -> Journal -> [DatedEntry] -> [(DatedEntry, Transaction)]
datedEntriesToTransactions today hledgerCliOpts journal datedEntries =
  let datedEntriesAndMaybeSimilarTransactions =
        zipDatedEntryAndSimilarTransaction today hledgerCliOpts journal <$>
        datedEntries
      datedEntriesAndMaybeNewTransactions =
        fmap
          (uncurry zipDatedEntryAndNewTransaction)
          datedEntriesAndMaybeSimilarTransactions
      datedEntriesAndTransactions =
        fmap (uncurry nothingTransactionToTempTransaction) datedEntriesAndMaybeNewTransactions
  in datedEntriesAndTransactions

nothingTransactionToTempTransaction :: DatedEntry -> Maybe Transaction -> (DatedEntry, Transaction)
nothingTransactionToTempTransaction datedEntry (Just transaction) = (datedEntry, transaction)
nothingTransactionToTempTransaction datedEntry Nothing = (datedEntry, createTempTransaction datedEntry)

zipDatedEntryAndSimilarTransaction :: Day
                                   -> CliOpts
                                   -> Journal
                                   -> DatedEntry
                                   -> (DatedEntry, Maybe Transaction)
zipDatedEntryAndSimilarTransaction today hledgerCliOpts journal =
  applyFuncAndZip $
  datedEntryToMaybeSimilarTransaction today hledgerCliOpts journal

zipDatedEntryAndNewTransaction :: DatedEntry
                               -> Maybe Transaction
                               -> (DatedEntry, Maybe Transaction)
zipDatedEntryAndNewTransaction datedEntry Nothing = (datedEntry, Nothing)
zipDatedEntryAndNewTransaction datedEntry (Just similarTransaction) =
  ( datedEntry
  , datedEntryAndSimilarTransactionToRealTransaction
      datedEntry
      similarTransaction)

datedEntryToMaybeSimilarTransaction :: Day
                                    -> CliOpts
                                    -> Journal
                                    -> DatedEntry
                                    -> Maybe Transaction
datedEntryToMaybeSimilarTransaction today hledgerCliOpts journal datedEntry =
  let entryDesc = (description :: DatedEntry -> Text) datedEntry
  in getSimilarTransaction today hledgerCliOpts journal entryDesc

datedEntryAndSimilarTransactionToRealTransaction :: DatedEntry
                                                 -> Transaction
                                                 -> Maybe Transaction
datedEntryAndSimilarTransactionToRealTransaction datedEntry similarTransaction =
  case paccount <$> tpostings similarTransaction of
    [account1, account2] -> do
      assetsCashAccount <- findAssetsCashAccountName account1 account2
      expensesAccount <- findExpensesAccountName account1 account2
      pure $ createNewTransaction datedEntry assetsCashAccount expensesAccount
    _ -> Nothing

createNewTransaction :: DatedEntry -> AccountName -> AccountName -> Transaction
createNewTransaction datedEntry assetsCashAccount expensesAccount =
  let entryAmount = datedEntryAmountToQuantity datedEntry
      expensesAmount = num entryAmount
      assetsCashAmount = num (negate entryAmount)
      expensesPosting =
        accountAndAmountToPosting expensesAccount expensesAmount
      assetsCashPosting =
        accountAndAmountToPosting assetsCashAccount assetsCashAmount
  in nulltransaction
     { tdate = (day :: DatedEntry -> Day) datedEntry
     , tdescription = (description :: DatedEntry -> Text) datedEntry
     , tpostings = [assetsCashPosting, expensesPosting]
     }

createTempTransaction :: DatedEntry -> Transaction
createTempTransaction datedEntry =
  let transaction = createNewTransaction datedEntry "assets:cash" "expenses:_"
  in transaction {tcomment = "TODO this transaction"}

positiveIntToQuantity :: PositiveInt -> Quantity
positiveIntToQuantity = fromIntegral . unPositiveInt

datedEntryAmountToQuantity :: DatedEntry -> Quantity
datedEntryAmountToQuantity =
  positiveIntToQuantity . (amount :: DatedEntry -> PositiveInt)

accountAndAmountToPosting :: AccountName -> Amount -> Posting
accountAndAmountToPosting accountName amount =
  nullposting {paccount = accountName, pamount = Mixed [amount]}

findAssetsCashAccountName :: AccountName -> AccountName -> Maybe AccountName
findAssetsCashAccountName "assets:cash" _ = Just "assets:cash"
findAssetsCashAccountName _ "assets:cash" = Just "assets:cash"
findAssetsCashAccountName _ _ = Nothing

findExpensesAccountName :: AccountName -> AccountName -> Maybe AccountName
findExpensesAccountName account1 account2
  | "expenses" `isPrefixOf` account1 = Just account1
  | "expenses" `isPrefixOf` account2 = Just account2
  | otherwise = Nothing

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
  simpleEntriesOnDate <-
    fromEitherOrM eitherSimpleEntriesOnDate $ \parseError ->
      die $ "Got parser error: " <> tshow parseError
  let simpleEntriesOnDateSkipFirst = tailSafe simpleEntriesOnDate
  pure $
    concatMap simpleEntriesOnDateToDatedEntries simpleEntriesOnDateSkipFirst

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
      posInt <- positiveIntParser
      spaces
      desc <- many1 $ noneOf "\n\r"
      spaces
      let simpleEntry = SimpleEntry posInt $ pack desc
      pure simpleEntry

dayParser :: Day -> MyParser Day
dayParser today = do
  PositiveInt month <- positiveIntParser
  void $ char '/'
  PositiveInt day <- positiveIntParser
  let year = calcYear today month
      dayString = show year <> "-" <> pad month <> "-" <> pad day
      (maybeDay :: Maybe Day) = parseTimeM True defaultTimeLocale "%F" dayString
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

die :: MonadIO m => Text -> m a
die msg = do
  putStrLn $ "ERROR: " <> msg
  liftIO exitFailure

applyFuncAndZip :: (a -> b) -> a -> (a, b)
applyFuncAndZip f a = (a, f a)

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:t) = t

-----------
-- Types --
-----------

-- | Newtype wrapper for 'Int's that are above 0.
newtype PositiveInt = PositiveInt
  { unPositiveInt :: Int
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | Read in a 'PositiveInt' from a 'Text'.
positiveInt :: Text -> Either Text PositiveInt
positiveInt inputText =
  let (maybeInt :: Maybe Int) = readMay inputText
  in case maybeInt of
       Nothing ->
         Left $ "Cannot read input \"" <> inputText <> "\" as a positive int."
       Just int ->
         if int > 0
           then pure $ PositiveInt int
           else Left $
                "input \"" <> inputText <>
                "\" is less than 0. Not a positive int."

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
