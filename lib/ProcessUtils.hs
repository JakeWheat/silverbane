{-
a simple variation on System.Process.readProcess

takes a command line as a string, then the process library can
   handle quoted arguments however it does

combines stdout and stderr so it looks the same as if you run it on
the command line. I have absolutely no idea why this isn't a standard
function in these sorts of libraries.

-}
{-# LANGUAGE RankNTypes #-}
module ProcessUtils
    (myReadProcess
    ) where

import System.Process
    (CreateProcess(..)
    ,createPipe
    ,StdStream(..)
    ,shell
    ,waitForProcess
    ,createProcess
    )
import System.IO
    (hGetContents
    ,hPutStr)

import System.Exit (ExitCode(..))
import qualified Control.Concurrent.Async as A

import Control.DeepSeq (rnf)
import qualified Control.Exception as C

myReadProcess ::  Maybe String -> String -> String -> IO (ExitCode, String)
myReadProcess
    newwd
    -- ^ optional new working directory to execute process in
    cmdline
    -- ^ process command line
    input
    -- ^ standard input
    = do
    (hr, hw) <- createPipe
    cp <- createProcess (shell cmdline)
                                       {cwd = newwd
                                       , std_out = UseHandle hw
                                       , std_err = UseHandle hw
                                       , std_in = CreatePipe
                                       }
    case cp of
        (Just inh, Nothing, Nothing, hproc) -> do
            -- not quite sure if this is right:
            -- get contents lazily
            output <- hGetContents hr
            -- launch another thread to put to stdin
            a1 <- A.async $ hPutStr inh input
            -- wait for it to finish:
            -- hopefully it doesn't block on the hPutStr because the
            -- lazy hGetContents isn't reading when it gets data, but
            -- waiting for the content to be used? This seems to be
            -- what system.process code is doing to avoid this:
            C.evaluate $ rnf output
            -- there's all sorts of other error handling, sigpipe ignoring,
            -- and stuff missing here
            _ <- A.wait a1
            -- wait for the process to exit
            ec <- waitForProcess hproc
            return (ec, output)
            -- the System.Process code does a lot more, but it's complex to copy and adapt
        (Nothing, _, _,_) -> error "myReadProcess: Failed to get a stdin handle."
        (_, Just {}, _,_) -> error "myReadProcess: Unexpectedly got a stdout handle."
        (_, _, Just {}, _) -> error "myReadProcess: Unexpectedly got a stderr handle."
