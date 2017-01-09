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

-- | Look for the @HLEDGER_DEFAULT_LEDGER@ environment variable, and set it as
-- the 'file_' field in a default set of 'CliOpts'.
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
  withJournalDo hledgerCliOpts (writeTransactions datedInputEntries)

-- | Get the most recent 'Transaction' that is the most similar to the
-- description.
--
-- Returns 'Nothing' if there are no similar transactions
getSimilarTransaction
  :: Day -- ^ Today
  -> CliOpts
  -> Journal
  -> Text -- ^ Description to use to search in the journal.
  -> Maybe Transaction
getSimilarTransaction today hledgerCliOpts journal desc =
  let reportOpts = reportopts_ hledgerCliOpts
      query = queryFromOptsOnly today reportOpts
      historyMatches = transactionsSimilarTo journal query desc
      bestMatch = listToMaybe historyMatches
  in fmap snd bestMatch

-- | Take a list of 'DatedEntry's, figure out similar 'Transaction's for each,
-- replace the 'tdescription' to the 'description' of the 'DatedEntry', and
-- write those new 'Transaction's to the 'Journal' 'file_'.
--
-- If we can't figure out a similar 'Transaction' (or the similar 'Transaction'
-- doesn't meet some requirements), then just output a dummy @TODO@
-- 'Transaction'.
writeTransactions :: MonadIO m => [DatedEntry] -> CliOpts -> Journal -> m ()
writeTransactions datedEntries hledgerCliOpts journal = do
  today <- liftIO getCurrentDay
  let transactions =
        snd <$>
        datedEntriesToTransactions
          today
          hledgerCliOpts
          journal
          datedEntries
  traverse_ (liftIO . journalAddTransaction journal hledgerCliOpts) transactions

-- | Take a list of 'DatedEntry's and turn them into a list of 'Transaction's
-- to write into the 'Journal'.
--
-- Turn a 'DatedEntry' into a 'Transaction' by finding a similar 'Transaction'.
-- If there are no similar 'Transaction's (or there are other errors with the
-- similar 'Transaction'), then turn the 'DatedEntry' into a temporary @TODO@
-- 'Transaction'.
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

-- | Takes a 'DatedEntry' and a 'Maybe' 'Transaction'.  If the 'Maybe'
-- 'Transaction' is 'Just', then just return a tuple of the 'DatedEntry' and
-- the 'Transaction'.  If the 'Maybe' 'Transaction' is 'Nothing', then use
-- 'createTempTransaction' to create a temporary @TODO@ 'Transaction' that
-- needs to be filled in by hand.
nothingTransactionToTempTransaction
  :: DatedEntry
  -> Maybe Transaction  -- ^ Possibly a real 'Transaction'.
  -> (DatedEntry, Transaction) -- ^ Tuple of a 'DatedEntry' and either a real
                               -- 'Transaction', or a temporary @TODO@
                               -- 'Transaction' that needs to be filled in by
                               -- hand.
nothingTransactionToTempTransaction datedEntry maybeTransaction =
  (datedEntry, fromMaybe (createTempTransaction datedEntry) maybeTransaction)

-- | Uses 'datedEntryToMaybeSimilarTransaction' with the 'DatedEntry' passed
-- in to find a 'Transaction' with a 'tdescription' similar to the
-- 'DatedEntry's 'description'.  Returns both the 'DatedEntry' and the similar
-- 'Transaction'.
zipDatedEntryAndSimilarTransaction :: Day
                                   -> CliOpts
                                   -> Journal
                                   -> DatedEntry
                                   -> (DatedEntry, Maybe Transaction)
zipDatedEntryAndSimilarTransaction today hledgerCliOpts journal datedEntry =
  let maybeSimilarTransaction =
        datedEntryToMaybeSimilarTransaction
          today
          hledgerCliOpts
          journal
          datedEntry
  in (datedEntry, maybeSimilarTransaction)

-- | In @'zipDatedEntryAndNewTransaction' datedEntry maybeSimilarTransaction@,
-- if @maybeSimilarTransaction@ is 'Nothing', then just return
-- @(datedEntry, Nothing)@.
--
-- But if @maybeSimilarTransaction@ is @'Just' similarTransaction@, then call
-- 'datedEntryAndSimilarTransactionToRealTransaction' with @similarTransaction@
-- in order to create a real 'Transaction' based on the similar 'Transaction'.
zipDatedEntryAndNewTransaction
  :: DatedEntry
  -> Maybe Transaction -- ^ Similar transaction
  -> (DatedEntry, Maybe Transaction) -- ^ Tuple of 'DataEntry' that was passed
                                     -- in, and a real 'Transaction' based on
                                     -- the 'DataEntry' and similar
                                     -- 'Transaction' passed in.
zipDatedEntryAndNewTransaction datedEntry Nothing = (datedEntry, Nothing)
zipDatedEntryAndNewTransaction datedEntry (Just similarTransaction) =
  ( datedEntry
  , datedEntryAndSimilarTransactionToRealTransaction
      datedEntry
      similarTransaction)

-- | Find a similar 'Transaction' for a given 'DatedEntry'. Uses
-- 'getSimilarTransaction'.
--
-- If a similar transaction can't be found, return 'Nothing'.
datedEntryToMaybeSimilarTransaction
  :: Day -- ^ Today
  -> CliOpts
  -> Journal
  -> DatedEntry -- ^ Use the 'description' from this to find a similar
                -- 'Transaction'.
  -> Maybe Transaction
datedEntryToMaybeSimilarTransaction today hledgerCliOpts journal datedEntry =
  let entryDesc = (description :: DatedEntry -> Text) datedEntry
  in getSimilarTransaction today hledgerCliOpts journal entryDesc

-- | Convert a 'DatedEntry' and a similar 'Transaction' to a real 'Transaction'.
--
-- If there is no @\"assets:cash\"@ and @\"expenses:*\"@ account names in the
-- similar 'Transaction', then return 'Nothing'.
datedEntryAndSimilarTransactionToRealTransaction
  :: DatedEntry
  -> Transaction -- ^ A 'Transaction' that is similar to the description in the
                 -- 'DatedEntry'.
  -> Maybe Transaction
datedEntryAndSimilarTransactionToRealTransaction datedEntry similarTransaction =
  case paccount <$> tpostings similarTransaction of
    [account1, account2] -> do
      assetsCashAccount <- findAssetsCashAccountName account1 account2
      expensesAccount <- findExpensesAccountName account1 account2
      pure $ createNewTransaction datedEntry assetsCashAccount expensesAccount
    _ -> Nothing


-- | Create a new 'Transaction' based on the 'DatedEntry' with the given
-- 'AccountName's.
createNewTransaction
  :: DatedEntry
  -> AccountName -- ^ This 'AccountName' should be @\"assets:cash\"@.
  -> AccountName  -- ^ This 'AccountName' should start with @expenses@.
  -> Transaction
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

-- | Create a temporary @TODO@ 'Transaction' that needs to filled in by hand
-- based on this 'DatedEntry'.
createTempTransaction :: DatedEntry -> Transaction
createTempTransaction datedEntry =
  let transaction = createNewTransaction datedEntry "assets:cash" "expenses:_"
  in transaction {tcomment = "TODO this transaction"}

-- | Turn a 'PositiveInt' into a 'Quantity'.
positiveIntToQuantity :: PositiveInt -> Quantity
positiveIntToQuantity = fromIntegral . unPositiveInt

-- | Pull the 'amount' out of a 'DatedEntry' and turn it into a 'Quantity'.
datedEntryAmountToQuantity :: DatedEntry -> Quantity
datedEntryAmountToQuantity =
  positiveIntToQuantity . (amount :: DatedEntry -> PositiveInt)

-- | Turn an 'AccountName' and an 'Amount' into a 'Posting'.  This is a simple
-- way to create a 'Posting'.
accountAndAmountToPosting :: AccountName -> Amount -> Posting
accountAndAmountToPosting accountName amount =
  nullposting {paccount = accountName, pamount = Mixed [amount]}

-- | Given two 'AccountName's, figure out which one is @\"assets:cash\"@.
-- Return 'Nothing' if neither is.
findAssetsCashAccountName :: AccountName -> AccountName -> Maybe AccountName
findAssetsCashAccountName "assets:cash" _ = Just "assets:cash"
findAssetsCashAccountName _ "assets:cash" = Just "assets:cash"
findAssetsCashAccountName _ _ = Nothing

-- | Given two 'AccountName's, figure out which one has a prefix of
-- @\"expenses\"@.  Return 'Nothing' if neither does.
findExpensesAccountName :: AccountName -> AccountName -> Maybe AccountName
findExpensesAccountName account1 account2
  | "expenses" `isPrefixOf` account1 = Just account1
  | "expenses" `isPrefixOf` account2 = Just account2
  | otherwise = Nothing

--------------------------------
-- Parse Command Line Options --
--------------------------------

-- | Data type representing the command line options.
data Options = Options
  { inputFilePath :: FilePath -- ^ Input file path for the file that comes from
                              -- Google Keep.
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

optionsParser :: Parser Options
optionsParser = Options <$> argument str (metavar "INPUT_FILE")

-- | Parse the command line arguments into 'Options'.
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

-- | Call 'readFile' on the 'inputFilePath' from 'Options'.
readInputFile :: MonadIO m => Options -> m Text
readInputFile = readFile . inputFilePath

-- | Read a list of 'SimpleEntriesOnDate' from the 'inputFilePath' in
-- 'Options', and then convert them to a list of 'DatedEntry's.
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

-- | Parse the input file passed in as 'Text'.
parseInputFile
  :: Day  -- ^ Current 'Day'. Today.
  -> Options -- ^ Options with the 'inputFilePath'.
  -> Text -- ^ Entire text of the input file.
  -> Either ParseError [SimpleEntriesOnDate]
parseInputFile today options inputFile =
  parse (simpleEntriesOnDatesParser today) (inputFilePath options) inputFile

-- | Run 'simpleEntriesOnDateParser' multiple times with 'many1'.
simpleEntriesOnDatesParser
  :: Day  -- ^ Current 'Day'. Today.
  -> MyParser [SimpleEntriesOnDate]
simpleEntriesOnDatesParser today = spaces *> many1 (simpleEntriesOnDateParser today)

-- | Parse a 'SimpleEntriesOnDate'.
simpleEntriesOnDateParser
  :: Day  -- ^ Current 'Day'. Today.
  -> MyParser SimpleEntriesOnDate
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

-- | Parse a 'Day' from a value like @12/28@.
--
-- Uses 'calcYear' to figure out whether the year for the parsed 'Day' should
-- be this year or the previous year.
dayParser
  :: Day -- ^ This input 'Day' is the 'Day' for today.  It is used to determine
         -- whether the output 'Day's year will be this year or the previous
         -- year.
  -> MyParser Day
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

-- | Parse a 'PostiveInt'.
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

-- | Print out the error message passed in and then call 'exitFailure'.
die :: MonadIO m => Text -> m a
die msg = do
  putStrLn $ "ERROR: " <> msg
  liftIO exitFailure

-- | Safe version of 'tail'.  Returns '[]' if the input list is empty.
tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:t) = t

-- | Calculate the year to use for an input month.  If it is currently January
-- or February, and the input month is November or December, then use the
-- previous year.  Otherwise, use the current year.
calcYear :: Day -> Int -> Integer
calcYear today month =
  let (yearForToday, monthForToday, _) = toGregorian today
  in if (monthForToday == 1 || monthForToday == 2) && (month == 11 || month == 12)
       then yearForToday - 1
       else yearForToday


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

-- | This is just a 'PostiveInt' 'amount' with a description.  Used in
-- 'SimpleEntriesOnDate'.
data SimpleEntry = SimpleEntry
  { amount :: PositiveInt
  , description :: Text
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | This is the main type read from the input file.  It is a 'Day' with a
-- list of 'SimpleEntry's.
data SimpleEntriesOnDate = SimpleEntriesOnDate
  { day :: Day
  , simpleEntries :: [SimpleEntry]
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | A list of 'SimpleEntry's will get turned into this data type, which is
-- used throughout most of the code.
data DatedEntry = DatedEntry
  { day :: Day
  , amount :: PositiveInt
  , description :: Text
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | Convert a 'Day' and a 'SimpleEntry' to a 'DatedEntry'.
simpleEntryToDatedEntry :: Day -> SimpleEntry -> DatedEntry
simpleEntryToDatedEntry day SimpleEntry {amount, description} =
  DatedEntry day amount description

-- | Convert a 'SimplEntriesOnDate' to a list of 'DatedEntry's.
simpleEntriesOnDateToDatedEntries :: SimpleEntriesOnDate -> [DatedEntry]
simpleEntriesOnDateToDatedEntries SimpleEntriesOnDate {day, simpleEntries} =
  simpleEntryToDatedEntry day <$> simpleEntries
