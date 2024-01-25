{-

Parse a document to extract all the recognised sections and turn them
into assertions.

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Parse
    (parseFile
    ,prettyError

    -- testing
    ,ValidatedHeader(..)
    ,SessionOptions(..)
    ,myRunParse
    ,header
    ,sessionBody
    ,inlineCmdSessionBody
    ,file
    ) where

import           Data.Char                       (isSpace)
--import           Data.Functor.Identity           (Identity)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
--import qualified Data.Text.IO                    as T
import           Data.Void                       (Void)

import           Control.Monad                   (void, join)
import           Text.Megaparsec                 (ParseErrorBundle, ParsecT,
                                                  eof, errorBundlePretty,
                                                  takeWhile1P, takeWhileP, (<|>),
                                                  optional, chunk, match, many,
                                                  (<?>), between,choice,
                                                  setErrorOffset,getOffset, region,
                                                  lookAhead, manyTill,
                                                  getSourcePos, sourceLine,unPos,
                                                  runParserT,option
                                                    ,satisfy,notFollowedBy,
                                                 )
import           Text.Megaparsec.Char            (char)
import qualified Control.Monad.Permutations as P
import Data.Maybe (catMaybes)
import Control.Monad.State
    (State
    ,evalState
    ,put
    ,get
    )
import Assertion
    (FileChunk(..)
    ,EtFile(..)
    ,EtRun(..)
    ,EtSession(..)
    ,EtContinue(..)
    ,SessionLine(..)
    )
    
--import qualified Data.Set as Set


data ValidatedHeader
    = VHFile Text
    | VHFilePrefix Text
    | VHRun (Maybe Text) Text Bool -- cwd, command, false if non zero exit expected
    | VHRunInline (Maybe Text) Bool
    | VHSession SessionOptions
    | VHContinue
      deriving (Eq,Show)

data SessionOptions
    = SessionOptions
    {soCwd :: Maybe Text
    ,soCmdline :: Maybe Text -- nothing means inline cmd
    ,soPrompt :: Text
    ,soInitialText :: Maybe Bool
    ,soFilters :: [(Text,Text)]
    }
      deriving (Eq,Show)


type MyParseError = ParseErrorBundle Text Void

prettyError :: MyParseError -> Text
prettyError = T.pack . errorBundlePretty

-- the state is the prompt from the previous session
-- using when parsing continues
type Parser = ParsecT Void Text (State (Maybe Text))

myRunParse :: Parser a -> String -> Text -> Either MyParseError a
myRunParse p name input = evalState (runParserT p name input) Nothing

parseFile :: String -> Text -> Either MyParseError [FileChunk]
parseFile name input = myRunParse (file <* eof) name input

nonnewlinewhitespace :: Parser ()
nonnewlinewhitespace =
    void $ takeWhile1P (Just "whitespace") isNonNewlineWhitespace

isNonNewlineWhitespace :: Char -> Bool
isNonNewlineWhitespace c = isSpace c && c /= '\n'

lexeme :: Parser a -> Parser a
lexeme p = p <* void (optional nonnewlinewhitespace)


-- returns the line including the newline at the end if there is one
-- (this is not very efficient, but good enough for now)
-- if matches any et- attributes, returns all the attributes parsed out as well
header :: Parser (Maybe ValidatedHeader)
header = do
    void $ lexeme $ chunk "~~~~"
    vh' <- optional $ do
        void $ lexeme $ char '{'
        vh <- startingAttributes <|> pure Nothing
        void $ lexeme $ char '}'
        pure vh
    void (char '\n') <|> eof
    pure $ join vh'
  where
    startingAttributes :: Parser (Maybe ValidatedHeader)
    -- todo: this is a complete mess
    startingAttributes =
        choice
            [do
             vh <- choice
                  [VHFilePrefix <$> attributeRequiredValue "et-file-prefix"
                  ,VHFile <$> attributeRequiredValue "et-file"
                  ,do
                   x <- attributeOptionalValue "et-run"
                   (expectZeroExit,mcwd) <-
                        P.runPermutation $ (,)
                        <$> P.toPermutation (option True (False <$ attributeNoValue "et-non-zero-exit"))
                        <*> P.toPermutation (optional (attributeRequiredValue "et-cwd"))
                   
                   case x of
                       Nothing -> pure $ VHRunInline mcwd expectZeroExit
                       Just v -> pure $ VHRun mcwd v expectZeroExit
                  ,vhSession
                  ,VHContinue <$ attributeNoValue "et-continue"]
             endingAttributes (Just vh)
            ,errorAttribute
            ,anyAttribute *> startingAttributes
            ,pure Nothing]
    vhSession = do
        s <- attributeOptionalValue "et-session"
        pr <- attributeRequiredValue "et-prompt"

        let noInitialText = False <$ attributeNoValue "et-no-initial-text"
            etfilter =
                (,) <$> attributeRequiredValue "et-filter"
                <*> attributeRequiredValue "et-to"
        (itx, mcwd, fs) <- P.runPermutation $
            (,,) <$> P.toPermutationWithDefault Nothing (Just <$> noInitialText)
                <*> P.toPermutationWithDefault Nothing (Just <$> attributeRequiredValue "et-cwd")
                <*> P.toPermutation (many etfilter)

        pure $ VHSession $ SessionOptions mcwd s pr itx fs

    endingAttributes :: Maybe ValidatedHeader -> Parser (Maybe ValidatedHeader)
    endingAttributes mvh = choice
        [errorAttribute
        ,anyAttribute *> endingAttributes mvh
        ,pure mvh]
    -- match any recognised attribute and error
    errorAttribute = do
        o <- getOffset
        choice $
            map (\x -> attributeOptionalValue x *>
                    region (setErrorOffset o) (fail ("unexpected " <> T.unpack x)))
            ["et-file"
            ,"et-file-prefix"
            ,"et-run"
            ,"et-continue"
            ,"et-session"
            ,"et-prompt"
            ,"et-no-initial-text"
            ,"et-initial-text"
            ,"et-filter"
            ,"et-to"
            ,"et-non-zero-exit"
            ,"et-cwd"
            ]

    --------------------------------------
    -- attribute parsing

    anyAttribute = lexeme $ do
        void (iden <?> "attributeName")
        void $ optional $ equalsAndOptionalAttributeValue

    attributeNoValue :: Text -> Parser ()
    attributeNoValue nm = lexeme $ do
        specificIden nm
        choice
            [do
             void $ char '='
             choice
                [satisfy unquotedChar *> fail ("attribute should not have value: " <> T.unpack nm)
                ,pure ()]
            ,pure ()]

    attributeRequiredValue :: Text -> Parser Text
    attributeRequiredValue nm = lexeme $ do
        specificIden nm
        void $ char '='
        attributeValue

    attributeOptionalValue :: Text -> Parser (Maybe Text)
    attributeOptionalValue nm = lexeme $ do
        specificIden nm
        equalsAndOptionalAttributeValue
    
    equalsAndOptionalAttributeValue :: Parser (Maybe Text)
    equalsAndOptionalAttributeValue = lexeme $ do
        choice
            [do
             void $ char '='
             choice [Nothing <$ emptyAttributeValue
                    ,Just <$> attributeValue ] <?> "attribute value"
            ,pure Nothing]
    emptyAttributeValue :: Parser ()
    emptyAttributeValue = lexeme $
        -- parse attr= with nothing following the = as empty attribute
        -- the trick is, to make sure the = is followed by a valid char
        -- this has to be } or whitespace
        void (lookAhead (char '}')) <|> void (lookAhead (satisfy isNonNewlineWhitespace <?> "whitespace"))

    attributeValue = (iden <|> quoted <|> doubleQuoted) <?> "attribute value"

    specificIden :: Text -> Parser ()
    specificIden nm = do
        void (chunk nm) <* notFollowedBy (satisfy unquotedChar)

    iden :: Parser Text
    iden = takeWhile1P (Just "iden char") unquotedChar
    
    between' c = between (char c) (char c)
    quoted = between' '\'' $ takeWhileP (Just "character") (`notElem` ("'\n" :: [Char]))
    doubleQuoted = between' '"' $ takeWhileP (Just "character") (`notElem` ("\"\n" :: [Char]))
    -- don't let any of the unquoted chars be { or }
    unquotedChar = (`notElem` ("\"'>/={} \t\n" :: [Char]))


-- parse a mix of prompts and replies until ~~~~, if hit eof before
-- this, give an error
sessionBody :: Text -> Parser [SessionLine]
sessionBody prompt = line
  where
    line = endBody <|> promptLine <|> reply
    endBody = chunk "~~~~" *> (void (char '\n') <|> eof) *> pure []
    promptLine = do
        p <- chunk prompt *>
             (match $ do
                  void $ takeWhileP (Just "prompt text") (/='\n')
                  void $ char '\n')
        (Prompt (fst p):) <$> line
    anotherReplyLine = do
        void $ takeWhileP (Just "reply text") (/='\n')
        void $ char '\n'
        choice
            [lookAhead (chunk prompt) *> pure ()
            ,lookAhead (chunk "~~~~") *> pure ()
            ,anotherReplyLine]
    reply = do
        x <- match anotherReplyLine
        (Reply (fst x):) <$> line

-- parse the starting inline command, then a prompt body
inlineCmdSessionBody :: Text -> Parser (Text, [SessionLine])
inlineCmdSessionBody prompt = do
    void $ chunk "$ "
    cmd <- match (do
        void $ takeWhileP (Just "command text") (/= '\n')
        void $ char '\n')
           
    b <- sessionBody prompt
    pure $ (fst cmd, b)

simpleBody :: Parser Text
simpleBody = do
    b <- match lne
    void $ chunk "~~~~"
    void (char '\n') <|> eof
    pure $ fst b
 where
    lne = choice
        [do
         lookAhead $ do       
             void $ chunk "~~~~"
             void (char '\n') <|> eof
         pure ()
        ,do
         void $ takeWhileP (Just "text") (/='\n')
         void (char '\n')
         lne]


file :: Parser [FileChunk]
file = catMaybes <$> manyTill filex eof

getSourceLine :: Parser Int
getSourceLine = unPos . sourceLine <$> getSourcePos

filex :: Parser (Maybe FileChunk)
filex = choice
   [do
    o <- getSourceLine       
    vh <- header
    case vh of
        Nothing -> pure Nothing
        Just (VHFile nm) -> do
            bdy <- simpleBody
            pure . Just . FcFile $ EtFile o nm bdy
        Just (VHFilePrefix pr) -> Just . FcFile <$> fileInlineBody o pr
        Just (VHRun mcwd cmd zeroExit) -> do
            bdy <- simpleBody
            pure . Just . FcRun $ EtRun o mcwd cmd zeroExit bdy
        Just (VHRunInline mcwd zeroExit) -> Just . FcRun <$> runInline o mcwd zeroExit
        Just (VHSession (SessionOptions {..})) -> do
            put (Just soPrompt)
            Just . FcSession <$> session o soCwd soCmdline soPrompt soInitialText soFilters
        Just VHContinue -> do
            mprompt <- get
            case mprompt of
                Nothing -> region (setErrorOffset o) (fail "continue block without preceding session block")
                Just prompt -> Just . FcContinue <$> continue o prompt
   ,do
    void $ takeWhileP (Just "text") (/= '\n')
    void (char '\n') <|> eof
    pure Nothing]

session :: Int -> Maybe Text -> Maybe Text -> Text -> Maybe Bool -> [(Text,Text)] -> Parser EtSession
session ln mcwd mcmd prompt mInitialText filters = do
    cmd <- case mcmd of
        Just x -> pure x
        Nothing -> do
            void $ chunk "$ "
            cmdx <- takeWhile1P (Just "command line") (/= '\n')
            void $ char '\n'
            pure cmdx
    sl <- sessionBody prompt
    pure $ EtSession ln mcwd cmd prompt mInitialText filters sl

continue :: Int -> Text -> Parser EtContinue
continue ln prompt = do
    sl <- sessionBody prompt
    pure $ EtContinue ln sl


runInline :: Int -> Maybe Text -> Bool -> Parser EtRun
runInline ln mcwd zeroExit = do
    void $ chunk "$ "
    cmd <- takeWhile1P (Just "command line") (/= '\n')
    void $ char '\n'
    bdy <- simpleBody
    pure $ EtRun ln mcwd cmd zeroExit bdy
    
    
fileInlineBody :: Int -> Text -> Parser EtFile
fileInlineBody ln pr = do
    void $ many whitespaceOnlyLine
    void $ lexeme (chunk pr)
    fn <- choice
          [void (lexeme (chunk "File" <|> chunk "file"))
           *> lexeme (takeWhile1P (Just "filename") (not . isSpace))
          ,lexeme (takeWhile1P (Just "filename") (not . isSpace))]
    void $ char '\n'
    bdy <- simpleBody
    pure  $ EtFile ln fn bdy

whitespaceOnlyLine :: Parser ()
whitespaceOnlyLine = do
    void $ optional nonnewlinewhitespace
    void $ char '\n'
