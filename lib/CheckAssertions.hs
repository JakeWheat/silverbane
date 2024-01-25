{-

Take the assertions parsed from a document, and the results of running
the actions corresponding to them, and checks if the assertions pass,
producing usable error messages when they don't.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CheckAssertions
    (checkAssertions
    ,SilverbaneError(..)
    ,prettyError
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import RunActions
    (ActionedFileChunk(..)
    ,AFile(..)
    ,ARun(..)
    ,ASession(..)
    ,AContinue(..))

import Assertion
    (EtFile(..)
    ,EtRun(..)
    ,EtSession(..)
    ,EtContinue(..)
    ,SessionLine(..)
    )

import qualified DiffLibWrap as D
import Data.Char (isSpace)
import qualified RegexReplace as Re

------------------------------------------------------------------------------

data SilverbaneError
    = SilverbaneError String Int Text

prettyError :: SilverbaneError -> Text
prettyError (SilverbaneError fn ln msg) =
        T.pack fn <> ":" <> showT ln <> ":0: " <> msg

checkAssertions :: String -> [ActionedFileChunk] -> [SilverbaneError]
checkAssertions docPath afcs =
    concat $ flip map afcs $ \case
         AfcFile et af -> checkAFile docPath et af
         AfcRun et af -> checkARun docPath et af
         AfcSession et af -> checkASession docPath et af
         AfcContinue et af -> checkAContinue docPath et af

------------------------------------------------------------------------------

checkAFile :: String -> EtFile -> AFile -> [SilverbaneError]
checkAFile docPath et (AFileNotFound cn) =
    [SilverbaneError docPath (efStartLine et)
     $ "file not found: " <> efFilename et
        <> " (expected at " <> T.pack cn <> " )"]

checkAFile docPath et (AFileReadError cn err) =
    [SilverbaneError docPath (efStartLine et)
     $ "error reading file: " <> efFilename et
        <> " (" <> T.pack cn <> " )"
        <> ": " <> err]

checkAFile docPath et (AFileContents _ tgt) =
    if efBody et == tgt
    then []
    else -- todo: try stripping empty newlines from start and end of both files
         [SilverbaneError docPath (efStartLine et)
          $ "files don't match\n" <> showDiff (efBody et) tgt]

------------------------------------------------------------------------------

checkARun :: String -> EtRun -> ARun -> [SilverbaneError]
checkARun docPath et = \case
    ARunFailed err ->  [SilverbaneError docPath (erStartLine et)
                        $ "run failed: " <> err]
    ARun out
      | not (erZeroExit et) ->
        [SilverbaneError docPath (erStartLine et)
        $ "process didn't exit with non zero:\n" <> out]
      | otherwise -> checkoutput out
    ARunNonZero out
      | erZeroExit et ->
        [SilverbaneError docPath (erStartLine et)
        $ "process exited with non zero:\n" <> out]
      | otherwise -> checkoutput out
  where
    checkoutput out =
        if (erBody et) == out
        then []
        else [SilverbaneError docPath (erStartLine et)
              $ "output doesn't match:\n" <> showDiff (erBody et) out]

------------------------------------------------------------------------------

checkASession :: String -> EtSession -> ASession -> [SilverbaneError]
checkASession docPath et (ASessionFailed err) =
    [SilverbaneError docPath (esStartLine et)
        $ "run failed with unexpected issue:\n" <> err]
checkASession docPath et (ASession sls) =
    -- remove initial text from process if no initial text
    let sls' =
            case sls of
                (Reply _ : xs) | esInitialText et == Just False -> xs
                xs -> xs
    in compareSessions docPath (esStartLine et) (esPrompt et) (esFilters et) (esSessionLines et) sls'

------------------------------------------------------------------------------

checkAContinue :: String -> EtContinue -> AContinue -> [SilverbaneError]
checkAContinue docPath et AContinueNoSession =
    [SilverbaneError docPath (ecStartLine et) "continue without previous session"]
    
checkAContinue docPath et (AContinue etc sls) =
    compareSessions docPath (ecStartLine et) (esPrompt etc) (esFilters etc) (ecSessionLines et) sls
    

------------------------------------------------------------------------------

showDiff' :: Text -> Text -> Text -> Text
showDiff' src tgt showTgt =
    T.unlines
    ["----------"
    ,showTgt
    ,"----------"
    ,"diff from doc to actual output:"
    ,"----------"
    ,D.doDiff (D.D {D.fromName = "x"
                   ,D.toName = "y"
                   ,D.fromText = src
                   ,D.toText = tgt})
    ,"----------"]

showDiff :: Text -> Text -> Text
showDiff src tgt = showDiff' src tgt tgt

------------------------------------------------------------------------------

{-

Canonicalize sessions before comparing

if there's an issue: show the "original" output text,
then use the canonical versions for the diff

canonicalization is:

remove any prompts or replies that are all whitespace
remove leading and trailing whitespace from each prompt and reply
run the filters on each reply text

-}

compareSessions :: String -> Int -> Text -> [(Text,Text)] -> [SessionLine] -> [SessionLine] -> [SilverbaneError]
compareSessions docPath lne prompt filters docSls processSls =
    let docSls' = canonicalizeSls docSls
        processSls' = canonicalizeSls processSls
    in if docSls' == processSls'
       then []
       else [SilverbaneError docPath lne $
            "output doesn't match:\n"
            <> showDiff' (showSls docSls') (showSls processSls') (showSls processSls)
            {-<> "\n\n------docsls\n"
            <> showT docSls'
            <> "\n------processsls\n"
            <> showT processSls'-}]
  where
    canonicalizeSls :: [SessionLine] -> [SessionLine]
    canonicalizeSls =
        map filterItem . remoteWhitespaceOnly

    remoteWhitespaceOnly [] = []
    remoteWhitespaceOnly (Reply x : xs) | T.all isSpace x = remoteWhitespaceOnly xs
    remoteWhitespaceOnly (Prompt x : xs) | T.all isSpace x = remoteWhitespaceOnly xs
    remoteWhitespaceOnly (x:xs) = x : remoteWhitespaceOnly xs

    filterItem (Prompt p) = Prompt $ T.strip p
    filterItem (Reply r) = 
        let filterf :: Text -> Text
            filterf = let mf (re,sub) = Re.substitute (Re.compile re) sub
                      in foldr (.) id $ map mf filters
        in Reply $ filterf $ T.strip $ r

    showSls =
        T.unlines . map (\case
            Prompt p -> prompt <> p
            Reply r -> r)


showT :: Show a => a -> Text
showT = T.pack . show
