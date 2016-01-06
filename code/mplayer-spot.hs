#!/usr/bin/env stack
-- stack --resolver=lts-3.20 runghc --package=async --package=process --package=bytestring --package=text --package=conduit-extra --package=conduit-combinators

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

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (Concurrently(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Binary (encode, decode)
import Data.ByteString (ByteString)
import Data.Conduit (($$), Sink, Source, yield)
import Data.Conduit.Combinators (stderr, stdin, stdout)
import Data.Conduit.List as CL
import Data.Conduit.Process (ClosedStream(..), streamingProcess, waitForStreamingProcess)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Word (Word32, Word8, byteSwap32)
import Numeric (showHex)
import System.Environment (getArgs)
import System.IO (Handle)
import System.Process (shell)

-- Define the the types that should be defaulted to.  We can define one
-- type for string-like things, and one type for integer-like things.  It
-- doesn't matter what order they are in.
default (T.Text, Int)

main :: IO ()
main = do
    programArgs <- getArgs
    let mplayerArgs = unwords $ ["mplayer", "-identify", "-slave"] <> programArgs

    -- open the ropasaurauxrex binary
    (processStdin :: Sink ByteString IO (), processStdout :: Source IO ByteString, processStderr :: Source IO ByteString, processHandle) <-
        streamingProcess (shell mplayerArgs)
        -- streamingProcess (shell "yes")

    let stdinToMplayerStdin = stdin $$ processStdin
    let mplayerStdoutToStdout = processStdout $$ stdout
    let mplayerStderrToStderr = processStderr $$ stderr

    exitCode <- runConcurrently $
                    Concurrently mplayerStdoutToStdout *>
                    Concurrently mplayerStderrToStderr *>
                    Concurrently stdinToMplayerStdin *>
                    Concurrently (waitForStreamingProcess processHandle)

    print $ "Exiting with error code: " <> show exitCode

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

