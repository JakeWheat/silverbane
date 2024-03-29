{-

Take the assertions parsed from a document, and run the IO actions
corresponding to them (without any checking, which is done in
CheckAssertions). This includes reading files, running command lines
and interactive command lines.

-}
{-# LANGUAGE LambdaCase #-}
module RunActions
    (runActions
    ,ActionedFileChunk(..)
    ,AFile(..)
    ,ARun(..)
    ,ASession(..)
    ,AContinue(..)
    ) where

import GHC.Stack (HasCallStack)

import Assertion
    (FileChunk(..)
    ,EtFile(..)
    ,EtRun(..)
    ,EtSession(..)
    ,EtContinue(..)
    ,SessionLine(..)
    )
import qualified Pexpect as P
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO  as T
import System.Directory (doesFileExist, canonicalizePath)
import System.FilePath
    ((</>)
    ,takeDirectory
    )
import Control.Monad (void)
import Data.IORef
    (newIORef
    ,readIORef
    ,writeIORef
    --,modifyIORef
    ,IORef
    )
import Control.Monad.Reader
    (ReaderT
    ,runReaderT
    ,asks
    ,liftIO
    )
import System.Exit (ExitCode(..))
import Data.Maybe (mapMaybe)

import Control.Exception.Safe (tryAny)

import ProcessUtils (myReadProcess)

------------------------------------------------------------------------------

data ActionedFileChunk
    = AfcFile EtFile AFile
    | AfcRun EtRun ARun
    | AfcSession EtSession ASession
    | AfcContinue EtContinue AContinue
    deriving (Eq,Show)

data AFile
    = AFileNotFound String -- canonicalized name
    | AFileReadError String Text
    | AFileContents String Text
    deriving (Eq,Show)

data ARun
    = ARunFailed Text
    | ARun Text
    | ARunNonZero Text
    deriving (Eq,Show)

data ASession
    = ASessionFailed Text
    | ASession [SessionLine]
    deriving (Eq,Show)

data AContinue
    = AContinueNoSession
    | AContinue EtSession [SessionLine]
    deriving (Eq,Show)

------------------------------------------------------------------------------

type RunAction = ReaderT MyState IO

data MyState
    = MyState
    {documentPath :: String
    ,sessionState :: IORef (Maybe SessionState)
    }

data SessionState
    = SessionState
    {stHandle :: P.Pexpect
    ,stSess :: EtSession
    }
    
runActions :: String -> [FileChunk] -> IO [ActionedFileChunk]
runActions docPath fcs = do

    P.initPexpect

    st <- newIORef Nothing
    flip runReaderT (MyState docPath st) $ flip mapM fcs $ \case
        FcFile a -> AfcFile a <$> runFileAction a
        FcRun a -> AfcRun a <$> runRunAction a
        FcSession a -> AfcSession a <$> runSessionAction a
        FcContinue a -> AfcContinue a <$> runContinueAction a

------------------------------------------------------------------------------

runFileAction :: EtFile -> RunAction AFile
runFileAction et = do
    docPath <- asks documentPath
    sfn <- liftIO $ canonicalizePath (takeDirectory docPath </> T.unpack (efFilename et))
    e <- liftIO $ doesFileExist sfn
    if e
        then do
            -- todo: add try to readfile call
            src1 <- liftIO $ T.readFile sfn
            pure $  AFileContents sfn src1
        else pure $ AFileNotFound sfn

------------------------------------------------------------------------------

runRunAction :: EtRun -> RunAction ARun
runRunAction et = do
    docPath <- asks documentPath

    let wd = Just $ maybe (takeDirectory docPath)
                    ((takeDirectory docPath </>) . T.unpack)
                    (erCwd et)
    eres <- tryAny $ liftIO $ myReadProcess wd (T.unpack $ erCmd et) ""
    case eres of
        Left e -> pure $ ARunFailed $ showT e
        Right (ExitSuccess, res) -> pure $ ARun $ T.pack res
        Right (ExitFailure {}, res) -> pure $ ARunNonZero $ T.pack res

------------------------------------------------------------------------------

runSessionAction :: EtSession -> RunAction ASession
runSessionAction et = do
    sessRef <- asks sessionState
    do
        -- close old session if there is one
        x <- liftIO $ readIORef sessRef
        flip (maybe (pure ())) x $ \ss -> do
            void $ liftIO $ P.close $ stHandle ss
            liftIO $ writeIORef sessRef Nothing

    eh <- tryAny $ do
        docPath <- asks documentPath
        let wd = Just . T.pack $ maybe (takeDirectory docPath)
                  ((takeDirectory docPath </>) . T.unpack)
                  (esCwd et)
        h <- liftIO $ P.spawn wd (esCmd et)
        -- no idea how to get this so it catches the exception
        -- properly without doing this here, some sort of
        -- unwanted laziness somewher
        initialText <- liftIO $ P.expect h (esPrompt et)
        pure (h,initialText)
    case eh of
        Left e -> do
            -- todo: will this potentially leave exes running? want to clean
            -- up reliably
            liftIO $ writeIORef sessRef Nothing
            pure $ ASessionFailed $ showT e
        Right (h, initialText) -> do
            rs <- liftIO $ runSession h (esPrompt et) (esSessionLines et)
            liftIO $ writeIORef sessRef $ Just $ SessionState h et
            pure $ ASession (Reply initialText : rs)

------------------------------------------------------------------------------

runSession :: P.Pexpect -> Text -> [SessionLine] -> IO [SessionLine]
runSession h prompt sls = do
    -- extract the prompts
    let prs = flip mapMaybe sls $ \case
            Prompt p -> Just p
            Reply {} -> Nothing
    rs <- flip mapM prs $ \p -> do
        -- todo: another hack
        rep <- P.exchange h prompt (T.stripEnd p)
        -- todo: add some config to turn on debugging logs at runtime
        --putStrLn $ "Prompt " <> esPrompt et <> " [" <> p <> "]\nReply[" <> rep <> "]"
        --putStrLn $ show (p `T.isPrefixOf` rep
        --                ,T.stripEnd p `T.isPrefixOf` T.stripStart rep)
        pure [Prompt p, Reply rep]
    pure $ concat rs

------------------------------------------------------------------------------

runContinueAction :: EtContinue -> RunAction AContinue
runContinueAction et = do
    sessRef <- asks sessionState
    ms <- liftIO $ readIORef sessRef
    case ms of
        Nothing -> pure $ AContinueNoSession
        Just ss -> AContinue (stSess ss) <$>
            liftIO (runSession (stHandle ss) (esPrompt $ stSess ss) (ecSessionLines et))

------------------------------------------------------------------------------

_errorT :: HasCallStack => Text -> a
_errorT = error . T.unpack

showT :: Show a => a -> Text
showT = T.pack . show
