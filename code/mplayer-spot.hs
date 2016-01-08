#!/usr/bin/env stack
-- stack --resolver=lts-4.0 runghc --package=async --package=filepath --package=directory --package=process --package=bytestring --package=text --package=conduit-extra --package=conduit-combinators

-- This gives us an experience similar to @ghci@.  Raw values (1 and
-- 10 below) that implement the 'Num' type class will be defaulted to 'Int'
-- (specified with the @default@ command below).  Raw values (all strings)
-- that implement the 'IsString' typeclass will be defaulted to Text (also
-- specified with the @default@ command below).
--
-- Without this extension turned on, ghc will produce errors like this:
-- @
--  code.hs:27:25:
--      No instance for (Data.String.IsString a0)
--            arising from the literal ‘"-alF"’
-- @
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- Don't warn that things (like strings and numbers) are being defaulted to
-- certain types.  It's okay because this is just shell programming.
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- import Prelude hiding (takeWhile)

import Conduit (iterMC)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, tryReadMVar)
import Control.Exception (IOException, finally, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Attoparsec.ByteString.Char8
    ( Parser, char, notInClass, parseOnly, rational, skipSpace, skipWhile
    , string, takeWhile1
    )
import Data.Binary (encode, decode)
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString.Char8 as C8
import Data.Conduit ((=$=), ($$), Sink, Source, yield)
import Data.Conduit.Combinators (stderr, stdin, stdout)
import Data.Conduit.List as CL
import Data.Conduit.Process (ClosedStream(..), streamingProcess, waitForStreamingProcess)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Streaming.Process (StreamingProcessHandle)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Word (Word32, Word8, byteSwap32)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeFile)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (Handle)
import System.Process (proc)

-- Define the the types that should be defaulted to.  We can define one
-- type for string-like things, and one type for integer-like things.  It
-- doesn't matter what order they are in.
default (T.Text, Int)

data Config = Config { configMPlayerSpotRCDir :: FilePath
                     , configSpotsDir :: FilePath
                     , configIgnoreSeconds :: Float
                     }
    deriving Show

defaultConfig :: IO Config
defaultConfig = do
    homeDir <- getHomeDirectory
    let rcDir = homeDir </> ".mplayer-spot"
    let spotsDir = rcDir </> "spots"
    let ignoreSeconds = 10
    pure $ Config rcDir spotsDir ignoreSeconds

data MediaInfo = MediaInfo { mediaInfoLength :: Maybe Float
                           , mediaInfoFilename :: Maybe ByteString
                           , mediaInfoCurPos :: Maybe Float
                           }
    deriving Show

defaultMediaInfo :: MediaInfo
defaultMediaInfo = MediaInfo { mediaInfoLength = Nothing
                             , mediaInfoFilename = Nothing
                             , mediaInfoCurPos = Nothing
                             }

data MPlayer = MPlayer { mplayerStdin :: Sink ByteString IO ()
                       , mplayerStdout :: Source IO ByteString
                       , mplayerStderr :: Source IO ByteString
                       , mplayerProcHandle :: StreamingProcessHandle
                       }

createMPlayerProcess :: [String] -> IO MPlayer
createMPlayerProcess programArgs = do
    let mplayerArgs = ["-identify", "-slave"] <> programArgs
    (processStdin, processStdout, processStderr, processHandle) <-
        streamingProcess (proc "mplayer" mplayerArgs)
    return $! MPlayer processStdin processStdout processStderr processHandle

-- | Parser for a value prefixed by a bytestring.  Uses skipWhile to make it
-- faster.
genericParser :: forall a . ByteString -> Parser a -> ByteString -> Maybe a
genericParser str parser mplayerLine =
    either (const Nothing) Just $ parseOnly go mplayerLine
  where
    go :: Parser a
    go = do
        -- Skip until we find the first character of the prefix that we are looking for.
        skipWhile (/= C8.head str)
        -- Try to match the prefix. If it matches, run the parser.
        string str *> parser
            -- If it doesn't match, then strip the first character and recurse.
            <|> char (C8.head str) *> go

getLength :: ByteString -> Maybe Float
getLength = genericParser "ID_LENGTH=" rational

getFilename :: ByteString -> Maybe ByteString
getFilename = genericParser "ID_FILENAME=" $ takeWhile1 (notInClass "\n")

getCurPos :: ByteString -> Maybe Float
getCurPos = genericParser "A: " $ skipSpace *> rational

updateLength :: MVar MediaInfo -> ByteString -> IO ()
updateLength mediaInfoMVar mplayerLine = do
    maybeMediaInfo <- tryReadMVar mediaInfoMVar
    case maybeMediaInfo of
        Just (MediaInfo (Just _) _ _) -> pure ()
        _ ->
            case getLength mplayerLine of
                Nothing -> pure ()
                Just mediaLength ->
                    modifyMVar_ mediaInfoMVar $ updateMediaInfo mediaLength
  where
    updateMediaInfo :: Float -> MediaInfo -> IO MediaInfo
    updateMediaInfo mediaLength mediaInfo =
        pure mediaInfo { mediaInfoLength = Just mediaLength }

updateFilename :: MVar MediaInfo -> ByteString -> IO ()
updateFilename mediaInfoMVar mplayerLine = do
    maybeMediaInfo <- tryReadMVar mediaInfoMVar
    case maybeMediaInfo of
        Just (MediaInfo _ (Just _) _) -> pure ()
        _ ->
            case getFilename mplayerLine of
                Nothing -> pure ()
                Just mediaFilename ->
                    modifyMVar_ mediaInfoMVar $ updateMediaInfo mediaFilename
  where
    updateMediaInfo :: ByteString -> MediaInfo -> IO MediaInfo
    updateMediaInfo mediaFilename mediaInfo =
        pure mediaInfo { mediaInfoFilename = Just mediaFilename }

updateCurPos :: MVar MediaInfo -> ByteString -> IO ()
updateCurPos mediaInfoMVar mplayerLine =
    maybe (pure ()) (modifyMVar_ mediaInfoMVar . updateMediaInfo) $ getCurPos mplayerLine
  where
    updateMediaInfo :: Float -> MediaInfo -> IO MediaInfo
    updateMediaInfo mediaCurPos mediaInfo =
        pure mediaInfo { mediaInfoCurPos = Just mediaCurPos }

processMPlayerStdout :: MVar MediaInfo -> ByteString -> IO ()
processMPlayerStdout mediaInfoMVar mplayerLine = do
    updateLength mediaInfoMVar mplayerLine
    updateFilename mediaInfoMVar mplayerLine
    updateCurPos mediaInfoMVar mplayerLine

runMPlayerUpdateMediaInfo :: MPlayer -> MVar MediaInfo -> IO ()
runMPlayerUpdateMediaInfo mplayer mediaInfoMVar = do
    -- Read keypresses from stdin, translate to mplayer commands, and pipe to mplayer.
    let stdinToMplayerStdin = stdin $$ mplayerStdin mplayer

    -- Process mplayer's stdout to find filename, length, and current position.
    -- Print mplayer's stdout to our stdout.
    let mplayerStdoutToStdout = mplayerStdout mplayer $$
                                iterMC (processMPlayerStdout mediaInfoMVar) =$=
                                stdout


    -- Print mplayer's stderr to our stderr.
    let mplayerStderrToStderr = mplayerStderr mplayer $$ stderr

    -- Handle for controlling the mplayer process.
    let mplayerHandle = mplayerProcHandle mplayer

    -- Run all conduits concurrently.
    exitCode <- runConcurrently $
                    Concurrently mplayerStdoutToStdout *>
                    Concurrently mplayerStderrToStderr *>
                    Concurrently stdinToMplayerStdin *>
                    Concurrently (waitForStreamingProcess mplayerHandle)

createMPlayerSpotsDir :: Config -> IO ()
createMPlayerSpotsDir (Config rcDir spotsDir _) = do
    createDirectoryIfMissing True rcDir
    createDirectoryIfMissing True spotsDir

writeSpotFile :: Config -> MVar MediaInfo -> IO ()
writeSpotFile (Config _ spotsDir ignoreLength) mediaInfoMVar = do
    maybeMediaInfo <- tryReadMVar mediaInfoMVar
    case maybeMediaInfo of
        Just (MediaInfo (Just mediaLength) (Just filename) (Just exitPos)) -> do
            let spotFilename = spotsDir </> unpack (decodeUtf8 filename)
            writeSpotFile' mediaLength spotFilename exitPos
        _ -> putStrLn "When exiting, do not currently have all fields of media info, so cannot write out spot file."
  where
    writeSpotFile' :: Float -> FilePath -> Float -> IO ()
    writeSpotFile' mediaLength spotFilename exitPos
        | exitPos <= ignoreLength = do
            putStrLn $ "exit position is " <> show exitPos <> " seconds so not writing spot file (not far enough)"
        | exitPos >= (mediaLength - ignoreLength) = do
            putStrLn $ "exit position is " <> show exitPos <> " seconds so not writing spot file (too close to end)"
            removeOldSpotFile spotFilename
        | otherwise =
            writeFloatToFile spotFilename exitPos

    writeFloatToFile :: FilePath -> Float -> IO ()
    writeFloatToFile spotFilename exitPos = do
        putStrLn $ "writing to file: " <> spotFilename <> " (" <> show exitPos <> ")"
        writeFile spotFilename $ show exitPos

    removeOldSpotFile :: FilePath -> IO ()
    removeOldSpotFile =
        void . (try :: IO () -> IO (Either IOException ())) . removeFile

main :: IO ()
main = do
    -- read in program arguments
    programArgs <- getArgs

    -- create the config we will be using
    config <- defaultConfig

    -- create the .mplayer-spots directory
    createMPlayerSpotsDir config

    -- create the mplayer process
    mplayerProcess <- createMPlayerProcess programArgs

    -- create the MediaInfo MVar we will be using to do concurrent stuff
    mediaInfoMVar <- newMVar defaultMediaInfo

    finally (runMPlayerUpdateMediaInfo mplayerProcess mediaInfoMVar) $ do
        writeSpotFile config mediaInfoMVar
        maybeMediaInfo <- tryReadMVar mediaInfoMVar
        print $ "finished: " <> show maybeMediaInfo


    -- run our input and output conduits concurrently
    -- exitCode <- runConcurrently $
    --                 Concurrently mplayerStdoutToStdout *>
    --                 -- Concurrently stdinToMplayerStdin *>
    --                 Concurrently (waitForStreamingProcess processHandle)

    -- putStrLn $ "exitCode: " <> show exitCode

    -- -- send the exploit to ropasaurusrex's stdin
    -- yield (BL.toStrict paddedExploit) $$ processStdin

    -- -- read the addr of write in libc from ropasaurusrex's stdout
    -- writeAddrRaw <- fromJust <$> (processStdout $$ CL.head)

    -- -- calculate the value of 'system' based on the value of 'write'
    -- let writeAddr = toAddr . decode $ BL.fromStrict writeAddrRaw
    -- let systemAddr = toAddr (writeAddr - systemOffset)

    -- putStrLn $ showHex writeAddr ""
    -- putStrLn $ showHex systemOffset ""
    -- putStrLn $ showHex (toAddr systemAddr) ""

    -- -- send the calculated address of 'system' to ropasaurusrex's stdin
    -- yield (BL.toStrict $ encode systemAddr) $$ processStdin

    -- -- send the @/bin/sh@ string to ropasaurusrex's stdin
    -- yield "/bin/sh\0" $$ processStdin

    -- -- make two conduits that feed breaker.hs's stdin to ropasaurusrex's
    -- -- stdin, and ropasaurusrex's stdout to breaker.hs's stdout.  This lets us
    -- -- easily control the shell that will spawn.
    -- -- let input = CB.sourceHandle stdin $$ processStdin
    -- let stdinToRopasaurusrexStdin = stdin $$ processStdin
    --     ropasaurusrexStdoutToStdout = processStdout $$ stdout

    -- -- run our input and output conduits concurrently
    -- exitCode <- runConcurrently $
    --                 Concurrently stdinToRopasaurusrexStdin *>
    --                 Concurrently ropasaurusrexStdoutToStdout *>
    --                 Concurrently (waitForStreamingProcess processHandle)

    -- putStrLn $ "exitCode: " <> show exitCode

    -- BL.hPutStr stderr "\n"
    -- BL.hPutStr stderr "---------------\n"
    -- BL.hPutStr stderr "-- Finished. --\n"
    -- BL.hPutStr stderr "---------------\n\n"

