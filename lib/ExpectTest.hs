
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ExpectTest
    (expectTest
    ,ExpectTestError(..)
    ,prettyExpectError
    ) where

import qualified Prelude as Pr
import Prelude hiding (error, show, putStrLn)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (when, void)
import System.Directory (doesFileExist)
import System.Process (readProcess)
import Data.Maybe (mapMaybe)
import qualified Pexpect as P
import Data.Char (isSpace)
--import Debug.Trace (trace)

import Data.IORef
    (newIORef
    ,readIORef
    ,writeIORef
    ,modifyIORef
    )

import Parse
    (parseFile
    ,prettyError
    ,FileChunk(..)
    ,EtFile(..)
    ,EtRun(..)
    ,EtSession(..)
    ,SessionLine(..)
    ,EtContinue(..)
    )

data ExpectTestError
    = ExpectTestError String Int Text

prettyExpectError :: ExpectTestError -> Text
prettyExpectError (ExpectTestError fn ln msg) =
        T.pack fn <> ":" <> show ln <> ":0: " <> msg

expectTest :: String -> Text -> IO [ExpectTestError]
expectTest fn input = do

    --input <- T.readFile fn
    let ps = either (error . prettyError) id $ parseFile fn input

    P.initPexpect

    currentSpawn <- newIORef Nothing

    let runSession h prompt sls = do
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
        -- todo: strip the whitespace more accurately
        compareSessions tgt sls =
            let debugShow :: Text -> Text -> a -> a
                debugShow _a _b x =
                     {-trace (T.unpack $ "mismatch:\n[" <> T.strip _a <> "]\n[" <> T.strip _b <> "]")-}
                     x
                -- a final empty prompt is optional
                -- todo: reason about this better
                f [Prompt p] [] | T.all isSpace p = True
                f [Prompt p, Reply q] [] | T.all isSpace p
                                         , T.all isSpace q = True
                f [Reply q] [] | T.all isSpace q = True
                f [] [] = True
                f (Prompt t:ts) (Prompt b:bs) =
                    if T.stripEnd t == T.stripEnd b
                    then f ts bs
                    else debugShow t b $ False
                f (Reply t:ts) (Reply b:bs) =
                    if T.strip t == T.strip b
                    then f ts bs
                    else debugShow t b $ False
                f _a _b = debugShow (show (_a,_b)) "" False
            in f tgt sls


    -- lifes too short
    esr <- newIORef []

    let addError ln msg = 
            -- todo: set a ioref flag, so it can get the process exit code right
            modifyIORef esr (ExpectTestError fn ln msg:)

    
    flip mapM_ ps $ \case
        FcFile et -> do
            -- read the file
            let sfn = T.unpack $ efFilename et
            e <- doesFileExist sfn
            if e
                then do
                    src1 <- T.readFile sfn
                    -- compare with local
                    when (efBody et /= src1) $ do
                        addError (efStartLine et) "files don't match"
                    -- if different, try trimming leading and trailing whitespace lines?
                    -- I think just ask the user to get it right
                 else
                     addError (efStartLine et) $ "file not found: " <> efFilename et
        FcRun et ->
            case map T.unpack $ T.words (erCmd et) of
                (c:cs) -> do
                    -- todo: catch exceptions here and report as regular error
                    tgt <- readProcess c cs ""
                    when (erBody et /= T.pack tgt) $
                        addError (erStartLine et) $ "output doesn't match: " <> T.pack tgt
                [] -> addError (erStartLine et) "no command given"
        FcSession et -> do
            old <- readIORef currentSpawn
            case old of
                Nothing -> pure ()
                Just (p,_) -> do
                    void $ P.close p
                    writeIORef currentSpawn Nothing
            -- run the command
            h <- P.spawn (esCmd et)
            initialText <- P.expect h (esPrompt et)

            rs <- runSession h (esPrompt et) (esSessionLines et)
            let tgt = (if esInitialText et == Just True
                       then (Reply initialText :)
                       else id) rs
            let showIt = T.unlines $ flip map tgt $ \case
                    Prompt p -> esPrompt et <> T.strip p
                    Reply r -> T.strip r
            if compareSessions tgt (esSessionLines et)
                then pure ()
                else addError (esStartLine et) $ "output doesn't match:\n" <> showIt
            writeIORef currentSpawn (Just (h,esPrompt et))
        FcContinue et -> do
            mh <- readIORef currentSpawn
            case mh of
                Nothing -> addError (ecStartLine et) "continue without previous session"
                Just (h, prompt) -> do
                    tgt <- runSession h prompt (ecSessionLines et)
                    let showIt = T.unlines $ flip map tgt $ \case
                            Prompt p -> prompt <> T.strip p
                            Reply r -> T.strip r
                    if compareSessions tgt (ecSessionLines et)
                        then pure ()
                        else addError (ecStartLine et) $ "output doesn't match:\n" <> showIt
    es <- readIORef esr
    pure $ reverse es
   
-- todo: add callstack
error :: Text -> a
error = Pr.error . T.unpack

show :: Show a => a -> Text
show = T.pack . Pr.show