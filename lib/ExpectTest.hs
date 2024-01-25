
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
import Control.Monad (void, unless)
import System.Directory (doesFileExist, canonicalizePath)
import ProcessUtils (myReadProcess)

import System.Exit (ExitCode(..))
import Data.Maybe (mapMaybe)
import qualified Pexpect as P
import Data.Char (isSpace, ord)
import Control.Exception.Safe 
import Debug.Trace (trace)
import qualified RegexReplace as Re
import qualified DiffLibWrap as D

import System.FilePath

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
import GHC.Stack (HasCallStack)

--import Data.Text.ICU.Normalize2 (compareUnicode)

data ExpectTestError
    = ExpectTestError String Int Text

showDebugMismatches :: Bool
showDebugMismatches = False

prettyExpectError :: ExpectTestError -> Text
prettyExpectError (ExpectTestError fn ln msg) =
        T.pack fn <> ":" <> show ln <> ":0: " <> msg

textEqual a b = a == b -- compareUnicode [] a b == EQ

showDiff src tgt =
            "----------\n" <> tgt <> "\n----------\ndiff:\n----------\n" <>
                D.doDiff (D.D {D.fromName = "x"
                             ,D.toName = "y"
                             ,D.fromText = src
                             ,D.toText = tgt})
            <> "----------"

        -- todo: strip the whitespace more accurately
compareSessions prompt filters tgt sls = do
    let showSessionDiff prompt doc tgt =
            let showIt = T.unlines . map (\case
                            Prompt p -> prompt <> T.strip p
                            Reply r -> T.strip r)
            in showDiff (showIt doc) (showIt tgt)

        compareSessionsx filters tgt sls =
            let filterf :: Text -> Text
                filterf =
                    let mf (re,sub) = Re.substitute (Re.compile re) sub
                    in foldr (.) id $ map mf filters
                debugShow :: Text -> Text -> a -> a
                debugShow _a _b x =
                    if showDebugMismatches
                    then let comparebits a b =
                                 T.unwords
                                 [T.singleton a
                                 ,T.singleton b
                                 ,show (a == b)
                                 , show (ord a)
                                 , "    "
                                 , show (ord b)
                                 , "    "
                                 ]
                             showit = T.unlines $ zipWith comparebits
                                      (T.unpack $ T.strip _a) (T.unpack $ T.strip _b)
                         in trace (T.unpack $ "mismatch:\n[" <> T.strip _a <> "]\n["
                                   <> T.strip _b <> "]\n" <> showit) x
                    else x
    
                -- a final empty prompt is optional
                -- todo: reason about this better
                f [Prompt p] [] | T.all isSpace p = True
                f [Prompt p, Reply q] [] | T.all isSpace p
                                         , T.all isSpace q = True
                f [Reply q] [] | T.all isSpace q = True
                f [] [] = True
                f (Prompt t:ts) (Prompt b:bs) =
                    if textEqual (T.stripEnd t) (T.stripEnd b)
                    then f ts bs
                    else debugShow t b $ False
                f (Reply t:ts) (Reply b:bs) =
                    let prepReply = filterf . T.strip
                    in --trace (T.unpack (prepReply t <> "\n\n" <> prepReply b)) $
                        if textEqual (prepReply t) (prepReply b)
                        then f ts bs
                        else debugShow t b $ False
                f _a _b = debugShow (show (_a,_b)) "" False
                -- replace prompt x, Reply all isspace, Prompt y with prompt x, prompt y
                f2 (Prompt x : Reply y : Prompt z : xs)
                    | T.all isSpace y =
                      f2 (Prompt x : Prompt z : xs)
                f2 (x:xs) = x : f2 xs
                f2 [] = []
                -- ignore leading replies that are all whitespace
                f1 (Reply t:ts) bs | T.all isSpace t = f1 ts bs
                f1 ts (Reply b:bs) | T.all isSpace b = f1 ts bs
                f1 ts bs = f (f2 ts) (f2 bs)
            in if f1 tgt sls
               then True
               else if showDebugMismatches
                    then trace (T.unpack ("program produced:\n" <> T.unlines (map show tgt) <> "\n\n/=\n\n"
                                <> "document says:\n" <> T.unlines (map show sls))) False
                    else False
    let x = compareSessionsx filters tgt sls
    if x then pure Nothing
        else pure $ Just $ "output doesn't match:\n"
                        -- todo: this doesn't take into account the filters,
                        -- not sure how should do that ...
                        <> showSessionDiff prompt sls tgt


-- TODO: in serious need of refactoring
expectTest :: String -> Text -> IO [ExpectTestError]
expectTest fn input = do
    let ps = either (error . prettyError) id $ parseFile fn input

    P.initPexpect
    D.initDiffLibWrap

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

    -- lifes too short
    esr <- newIORef []

    let addError ln msg = 
            -- todo: set a ioref flag, so it can get the process exit code right
            modifyIORef esr (ExpectTestError fn ln msg:)

    flip mapM_ ps $ \case
        FcFile et -> do
            -- read the file
            sfn <- canonicalizePath (takeDirectory fn </> T.unpack (efFilename et))
            let fn' = T.pack sfn
            e <- doesFileExist sfn
            
            if e
                then do
                    src1 <- T.readFile sfn
                    -- compare with local
                    unless (textEqual (efBody et) src1) $
                        addError (efStartLine et) $ "files don't match\n"
                          <> showDiff (efBody et) src1
                    -- if different, try trimming leading and trailing whitespace lines?
                    -- I think just ask the user to get it right
                else
                     addError (efStartLine et) $
                         "file not found: " <> efFilename et
                         <> " (expected at " <> fn' <> " )"
        FcRun et -> do
            let cwd = Just $ maybe (takeDirectory fn)
                    ((takeDirectory fn </>) . T.unpack)
                    (erCwd et)
            etgt <- tryAny $ myReadProcess cwd (T.unpack $ erCmd et) ""
            case etgt of
                Left e ->
                    addError (erStartLine et) $ "run failed with unexpected issue: " <> show e
                Right (ExitSuccess, tgt)
                    | not (erZeroExit et) ->
                      addError (erStartLine et) $ "process didn't exit with non zero:\n" <> T.pack tgt
                    | textEqual (erBody et) (T.pack tgt) -> pure ()
                    | otherwise ->
                      addError (erStartLine et) $ "output doesn't match:\n" <> showDiff (erBody et) (T.pack tgt)
                Right (ExitFailure {}, tgt)
                    | erZeroExit et ->
                      addError (erStartLine et) $ "process exited with non zero:\n" <> T.pack tgt
                    | textEqual (erBody et) (T.pack tgt) -> pure ()
                    | otherwise ->
                      addError (erStartLine et) $ "output doesn't match:\n" <> showDiff (erBody et) (T.pack tgt)
        FcSession et -> do
            old <- readIORef currentSpawn
            case old of
                Nothing -> pure ()
                Just (p,_,_) -> do
                    void $ P.close p
                    writeIORef currentSpawn Nothing
            -- run the command
            eh <- tryAny $ do
                let cwd = Just . T.pack $ maybe (takeDirectory fn)
                        ((takeDirectory fn </>) . T.unpack)
                        (esCwd et)
                h <- P.spawn cwd (esCmd et)
                -- no idea how to get this so it catches the exception
                -- properly without this
                initialText <- P.expect h (esPrompt et)
                pure (h,initialText)
            case eh of
                Left e -> do
                    addError (esStartLine et) $ "session process didn't start: " <> show e
                    writeIORef currentSpawn Nothing
                Right (h, initialText) -> do
                    rs <- runSession h (esPrompt et) (esSessionLines et)
                    let tgt = (if esInitialText et `elem` [Just True, Nothing]
                               then (Reply initialText :)
                               else id) rs
                    m <- compareSessions (esPrompt et) (esFilters et) tgt (esSessionLines et)
                    maybe (pure ()) (addError (esStartLine et)) m
                    writeIORef currentSpawn (Just (h,esPrompt et, esFilters et))
        FcContinue et -> do
            mh <- readIORef currentSpawn
            case mh of
                Nothing -> addError (ecStartLine et) "continue without previous session"
                Just (h, prompt, filters) -> do
                    tgt <- runSession h prompt (ecSessionLines et)
                    m <- compareSessions prompt filters tgt (ecSessionLines et)
                    maybe (pure ()) (addError (ecStartLine et)) m
    es <- readIORef esr
    pure $ reverse es

error :: HasCallStack => Text -> a
error = Pr.error . T.unpack

show :: Show a => a -> Text
show = T.pack . Pr.show
