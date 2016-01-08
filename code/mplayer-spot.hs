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
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, tryReadMVar)
import Control.Exception (IOException, finally, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString.Char8
    ( Parser, char, notInClass, parseOnly, rational, skipSpace, skipWhile
    , string, takeWhile1
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Conduit
    ( (=$=), ($$), Conduit, Producer, Sink, Source, awaitForever, yield )
import Data.Conduit.Combinators (stderr, stdin, stdout)
import Data.Conduit.Process (streamingProcess, waitForStreamingProcess)
import Data.Monoid ((<>))
import Data.Streaming.Process (StreamingProcessHandle)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (BufferMode(..), hSetBuffering)
import qualified System.IO as IO
import System.Process (proc)

-- Define the the types that should be defaulted to.  We can define one
-- type for string-like things, and one type for integer-like things.  It
-- doesn't matter what order they are in.
default (T.Text, Int)

-- | A config for our program.
data Config = Config { configMPlayerSpotRCDir :: FilePath -- ^ @~/.mplayer-spots@ dir
                     , configSpotsDir :: FilePath         -- ^ @~/.mplayer-spots/spots@ dir
                     , configIgnoreSeconds :: Float       -- how many seconds to ignore
                                                          -- in media before creating
                                                          -- spot file
                     }
    deriving Show

-- | Create a default 'Config'.
defaultConfig :: IO Config
defaultConfig = do
    homeDir <- getHomeDirectory
    let rcDir = homeDir </> ".mplayer-spot"
    let spotsDir = rcDir </> "spots"
    let ignoreSeconds = 180
    pure $ Config rcDir spotsDir ignoreSeconds

-- | Info about our media.
data MediaInfo = MediaInfo { mediaInfoLength :: Maybe Float          -- ^ length of the media
                           , mediaInfoFilename :: Maybe ByteString   -- ^ filename of the media
                           , mediaInfoCurPos :: Maybe Float          -- ^ current position
                           , mediaInfoAlreadySetOldLocation :: Bool  -- ^ whether we have already
                                                                     -- set the old location
                           }
    deriving Show

defaultMediaInfo :: MediaInfo
defaultMediaInfo = MediaInfo { mediaInfoLength = Nothing
                             , mediaInfoFilename = Nothing
                             , mediaInfoCurPos = Nothing
                             , mediaInfoAlreadySetOldLocation = False
                             }

-- | Datatype to hold mplayer's stdin, stdout, etc conduits.
data MPlayer = MPlayer { mplayerStdin :: Sink ByteString IO ()
                       , mplayerStdout :: Source IO ByteString
                       , mplayerStderr :: Source IO ByteString
                       , mplayerProcHandle :: StreamingProcessHandle
                       }

-- | Take in a bunch of arguments and use them to create the mplayer process.
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

-- | Parser for the length of the of the media.
getLength :: ByteString -> Maybe Float
getLength = genericParser "ID_LENGTH=" rational

-- | Parser for the filename of the of the media.
getFilename :: ByteString -> Maybe ByteString
getFilename = genericParser "ID_FILENAME=" $ takeWhile1 (notInClass "\n")

-- | Parser for the current location in the media.
getCurPos :: ByteString -> Maybe Float
getCurPos = genericParser "A:" $ skipSpace *> rational

updateLength :: MVar MediaInfo -> ByteString -> IO ()
updateLength mediaInfoMVar mplayerLine = do
    maybeMediaInfo <- tryReadMVar mediaInfoMVar
    case maybeMediaInfo of
        Just (MediaInfo (Just _) _ _ _) -> pure ()
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
        Just (MediaInfo _ (Just _) _ _) -> pure ()
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

-- | Try to read 3 different things from the mplayer stdout:
--
--   - length of the media
--   - media filename
--   - current position in the media
processMPlayerStdout :: MVar MediaInfo -> ByteString -> IO ()
processMPlayerStdout mediaInfoMVar mplayerLine = do
    updateLength mediaInfoMVar mplayerLine
    updateFilename mediaInfoMVar mplayerLine
    updateCurPos mediaInfoMVar mplayerLine

-- | 'Conduit' that reads in key presses (from stdin), and yields mplayer
-- commands.
--
-- In order for this to work right when hooked up to stdin, stdin should be set
-- to 'NoBuffering'.
sendMPlayerCommands :: Conduit ByteString IO ByteString
sendMPlayerCommands = awaitForever go
  where
    go :: ByteString -> Conduit ByteString IO ByteString
    go inputChar
        | inputChar == "q"       = yield "quit\n"
        | inputChar == "p"       = yield "pause\n"
        | inputChar == " "       = yield "pause\n"
        | inputChar == "\ESC[D"  = yield "seek -10\n"
        | inputChar == "\ESC[C"  = yield "seek 10\n"
        | inputChar == "\ESC[B"  = yield "seek -60\n"
        | inputChar == "\ESC[A"  = yield "seek 60\n"
        | inputChar == "\ESC[6~" = yield "seek -600\n"
        | inputChar == "\ESC[5~" = yield "seek 600\n"
        | otherwise =
            liftIO $ print $ "got keypress " <> inputChar <> " but don't know what to do with it"

-- | Producer that tries to read the filename from the @'MVar' 'MediaInfo'@,
-- and if it succeeds, tries to open the spot file and read in the saved
-- position.  Produce it as a value.
setOldLocation :: Config -> MVar MediaInfo -> Producer IO ByteString
setOldLocation config@(Config _ spotsDir _) mediaInfoMVar = do
    maybeMediaInfo <- liftIO $ tryReadMVar mediaInfoMVar
    case maybeMediaInfo of
        Just (MediaInfo _ _ _ True) -> pure ()
        Just (MediaInfo _ (Just filename) _ False) -> do
            filecontents <- liftIO $ try $ readFile $ spotsDir </> unpack (decodeUtf8 filename)
            case filecontents of
                Right oldLocation -> do
                    yield $ "seek " <> C8.pack oldLocation <> "\n"
                Left (_ :: IOException) -> pure ()
            liftIO $ modifyMVar_ mediaInfoMVar $ \mediaInfo -> pure mediaInfo { mediaInfoAlreadySetOldLocation = True }
        _ -> setOldLocation config mediaInfoMVar

-- | Run the mplayer process while doing 3 things:
--
--  - Read mplayer's stdout, looking for the length, filename, and current
--    position of the media.
--  - Once the filename has been found, look for a spot file, and if it exists,
--    make mplayer seek to the location.
--  - Take keys from stdin and translate them to mplayer commands.  Since mplayer
--    is in slave mode, it can't take in commands like normal.
runMPlayerUpdateMediaInfo :: Config -> MPlayer -> MVar MediaInfo -> IO ()
runMPlayerUpdateMediaInfo config mplayer mediaInfoMVar = do
    -- set stdin to not be buffered so we only get one character at a time
    hSetBuffering IO.stdin NoBuffering

    -- a conduit for sending the old location to mplayer
    let oldLocationToMplayerStdin =
            setOldLocation config mediaInfoMVar $$ mplayerStdin mplayer

    -- Read keypresses from stdin, translate to mplayer commands, and pipe to mplayer.
    let stdinToMplayerStdin = stdin $$
                              sendMPlayerCommands =$=
                              mplayerStdin mplayer

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
    runConcurrently $
        Concurrently mplayerStdoutToStdout *>
        Concurrently mplayerStderrToStderr *>
        Concurrently oldLocationToMplayerStdin *>
        Concurrently stdinToMplayerStdin *>
        Concurrently (waitForStreamingProcess mplayerHandle >>= exitWith)

-- | Create the .mplayer-spots/ and spots/ directories from the config file.
createMPlayerSpotsDir :: Config -> IO ()
createMPlayerSpotsDir (Config rcDir spotsDir _) = do
    createDirectoryIfMissing True rcDir
    createDirectoryIfMissing True spotsDir

-- | Write out a spot file to the spots directory if all the required fields
-- have been filled in the MediaInfo, and if our current position in the media
-- file is not too early or not too late.
writeSpotFile :: Config -> MVar MediaInfo -> IO ()
writeSpotFile (Config _ spotsDir ignoreLength) mediaInfoMVar = do
    maybeMediaInfo <- tryReadMVar mediaInfoMVar
    case maybeMediaInfo of
        Just (MediaInfo (Just mediaLength) (Just filename) (Just exitPos) _) -> do
            let spotFilename = spotsDir </> unpack (decodeUtf8 filename)
            writeSpotFile' mediaLength spotFilename exitPos
        _ -> putStrLn "When exiting, do not currently have all fields of media info, so cannot write out spot file."
  where
    -- | Check that the length is not too early or too late.  If it is not,
    -- then write out the spot file.
    writeSpotFile' :: Float -> FilePath -> Float -> IO ()
    writeSpotFile' mediaLength spotFilename exitPos
        | exitPos <= ignoreLength = do
            putStrLn $ "exit position is " <> show exitPos <> " seconds so not writing spot file (not far enough)"
        | exitPos >= (mediaLength - ignoreLength) = do
            putStrLn $ "exit position is " <> show exitPos <> " seconds so not writing spot file (too close to end)"
            removeOldSpotFile spotFilename
        | otherwise =
            writeFloatToFile spotFilename exitPos

    -- | Write a Float to a FilePath.
    writeFloatToFile :: FilePath -> Float -> IO ()
    writeFloatToFile spotFilename exitPos = do
        putStrLn $ "writing to file: " <> spotFilename <> " (" <> show exitPos <> ")"
        writeFile spotFilename $ show exitPos

    -- | Remove a file and ignore errors (like if the file doesn't exist).
    removeOldSpotFile :: FilePath -> IO ()
    removeOldSpotFile =
        void . (try :: IO () -> IO (Either IOException ())) . removeFile

main :: IO ()
main = do
    -- read in program arguments
    programArgs <- getArgs

    -- create the config we will be using
    config <- defaultConfig

    -- create the .mplayer-spots directory if it doesn't exist
    createMPlayerSpotsDir config

    -- create the mplayer process
    mplayerProcess <- createMPlayerProcess programArgs

    -- create the MediaInfo MVar we will be using to do concurrent stuff
    mediaInfoMVar <- newMVar defaultMediaInfo

    finally (runMPlayerUpdateMediaInfo config mplayerProcess mediaInfoMVar) $
        -- write the spot file after exiting
        writeSpotFile config mediaInfoMVar
