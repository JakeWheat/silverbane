
{-

Syntax overview:

It parses a file to a list of chunks:
text chunks
et-files
et-runs
et-sessions
et-continue

an et-file is a test where you have some inline text, and it will
check that a file contains the same text this is so you can use this
file in examples in documentation, and there's a test to make sure the
documentation version and the file being used are the same. I think
this is a better solution than trying to e.g. insert the file contents
at doc rendering time (which makes editing the doc source harder
because the file is not there), or generating the file from the doc at
test time (which introduces all sorts of mess)

an et-run section represents running a command line, and showing the
output from it

an et-session section represents running an interactive command line,
and exchanging messages with it

the text chunks are all the bits of the file between the above kinds
of chunks

it's designed to work with markdown. You can run it on code source, if
you can put markdown inside block comments or something similar

all interesting chunks are delimited with markdown "fences":
~~~~

~~~~

the metadata for a non text chunk is in the pandoc style attributes on
the first/opening fence:

~~~~{et-file=filename}
file contents
~~~~

alternative explicit filename to render. whitespace around the
filename is removed

~~~~{et-file-prefix='--'}
-- file Blah.hs
file contents
~~~~

whitespace before and after the inline file name is insignificant

~~~~{et-file-prefix='--'}

-- file Blah.hs

file contents
~~~~

the "file" part is optional. if you just write file as a mistake, it
should be a simple test fail that you can diagnose easily

~~~~{et-file-prefix='--'}

-- Blah.hs

file contents
~~~~

Run an cmdline and show the results

~~~~{et-run="echo this is a test"}
this is a test
~~~~

run a cmdline, inline variation

TODO: make this work with the session variation? maybe it's clearer to
keep this user syntax, and then can decide how to implement it separately

~~~~{et-run}
$ echo this is a test
this is a test
~~~~



~~~~{et-session="python3" et-prompt=">>>"}
>>> 1 + 2
3
~~~~

~~~~{et-continue}
>>> 4 + 5
9
>>> True
True
~~~~

sessions and continues can have multiple prompts, and text preceding
the first prompt. they can have no contents, just a prompt, or just
text, and can end with a prompt or text (from the point of view of the
parser)

A variation on the et-session is the explicit command invocation,
where the command line appears in the body. This is for documentation
that wants this rendered:

~~~~{et-session et-prompt=">>>"}
$ python3
>>> 1 + 2
3
~~~~

The first prompt is "$ "

It will ignore leading empty lines. Following ones will be ignored until
the first non empty line (this could be changed and adapted to easily)

~~~~{et-session et-prompt=">>>"}

$ python3

>>> 1 + 2
3
~~~~


newlines are not supported in any attributes, and continuation lines
are not currently supported for explicit command lines or prompt
text. This is a pretty harsh limitation, it's not clear how to get
round it without adding a bunch of crap to the syntax that you don't
want to see when rendered, and not really even when editing the source

if there are unrecognised attributes on this header line, they are
ignored (this could be revisited to make a whitelist to catch typos)

recognised attributes are like pre xml html (which is also what pandoc
supports):
attribute
attribute=value-with-allowed-chars
attribute="double quoted value"
attribute='single quoted value'

non quoted attribute values have alphanum + a few other chars (TBD)
will consider allowing escaped quotes if pandoc also supports
something like this

the attributes recognised are:

et-file-prefix='comment prefix'

et-file:
et-file=filename

et-run:
et-run='cmd line'

et-session:
et-session='cmd line'
et-prompt='prompt'
et-no-initial-text
et-initial-text
# these must be in pairs, and you can have multiple on a line:
et-filter="" et-to=""

et-continue:
et-continue

The system doesn't allow et-continue except when there is a previous
et-session in that file. This is not enforced by the parser - it's
checked later.

-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Parse where

import           Data.Char                       (isSpace)
import           Data.Functor.Identity           (Identity)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
--import qualified Data.Text.IO                    as T
import           Data.Void                       (Void)

import           Control.Monad                   (void, join)
import           Text.Megaparsec                 (ParseErrorBundle, ParsecT,
                                                  eof, errorBundlePretty, parse,
                                                  parseTest, satisfy,
                                                  takeWhile1P, takeWhileP, try, (<|>),
                                                  optional, chunk, match, many,
                                                  (<?>), between,choice,
                                                  setErrorOffset,getOffset, region,
                                                  failure
                                                    
                                                 )
import           Text.Megaparsec.Char            (char)
--import qualified Data.Set as Set

type Parser = ParsecT Void Text Identity
type MyParseError = ParseErrorBundle Text Void

prettyError :: MyParseError -> Text
prettyError = T.pack . errorBundlePretty

data FileChunk
    = TextChunk
    | FcFile EtFile
    | FcRun EtRun
    | FcSession EtSession
    | FcContinue EtContinue
      deriving (Eq,Show)
-- the int is the starting line of this chunk in the source

data EtFile
    = EtFile
    {etHeader :: Text -- the full header line
    ,etStartLine :: Int
    ,etFilename :: Text
    ,etBody :: Text}
      deriving (Eq,Show)

data EtRun
    = EtRun
    {etHeader :: Text
    ,etStartLine :: Int
    ,etCmd :: Text
    ,etBody :: Text}
      deriving (Eq,Show)

data EtSession
    = EtSession
    {etHeader :: Text
    ,etStartLine :: Int
    ,etCmd :: Either Text Text -- left is attribute, right is explicit
    ,etPrompt :: Text
    ,etNoInitialText :: Bool
    ,etFilters :: [(Text,Text)]}
      deriving (Eq,Show)

data EtContinue
    = EtContinue
    {etHeader :: Text
    ,etStartLine :: Int
    ,etBody :: Text}
      deriving (Eq,Show)


data ValidatedHeader
    = Regular -- means this header doesn't contain any specific
              -- directives for the program, it's just textchunkage
    | VHFile Text
    | VHFilePrefix Text
    | VHRun Text
    | VHRunInline
    | VHSession SessionOptions
    | VHContinue
      deriving (Eq,Show)

data SessionOptions
    = SessionOptions
    {soCmdline :: Maybe Text -- nothing means inline cmd
    ,soPrompt :: Text
    ,soInitialText :: Maybe Bool
    ,soFilters :: [(Text,Text)]
    }
      deriving (Eq,Show)


{-
parse a header line
-----------------

a header line is ~~~~

followed by one of the variations (on the same line):
e.g.

~~~~{et-file=filename}

arbitrary unrecognised attributes are ignored, so you can have other attributes
to control rendering as well as the ones which are directives for this program

a header line which doesn't have a matching } is an error (could adjust this to allow it)
a header line which doesn't contain any et- attributes doesn't represent the start
of a non text chunk block, it continues the current text block


look for a { with optional whitespace between the ~~~~ and {
if there's no } at the end of the line (with optional trailing whitespace)
  give an error
then parse the attributes
  find out exactly what the format is:
attribute
attribute=value
attribute='value'
attribute="value"

what characters can be used in attribute, and in the unquoted values?

HTML 5 spec:

```
Attributes for an element are expressed inside the element's start tag.

Attributes have a name and a value. Attribute names must consist of
one or more characters other than the space characters, U+0000 NULL,
U+0022 QUOTATION MARK ("), U+0027 APOSTROPHE ('), U+003E GREATER-THAN
SIGN (>), U+002F SOLIDUS (/), and U+003D EQUALS SIGN (=) characters,
the control characters, and any characters that are not defined by
Unicode. In the HTML syntax, attribute names, even those for foreign
elements, may be written with any mix of lower- and uppercase letters
that are an ASCII case-insensitive match for the attribute's name.

Attribute values are a mixture of text and character references,
except with the additional restriction that the text cannot contain an
ambiguous ampersand.  ```

So: use the attribute name syntax for unquoted values
forget about the ampersand stuff
an name cannot contain spaces, null, ", ', >, /, or =, or < 32
so anything other than one of these. assume there are no chars < 32
  and a name cannot start with } because of the pandoc markdown syntax

empty values are ok: attribute=
an unclosed value with no closing " or ' is an error

header attribute variations
et-file=filename
et-file-prefix=prefix
et-run=cmdline
et-run
et-session=cmd et-prompt=prompt
et-continue

for et-session, the two attributes have to appear in this order, one
right after the other

et-sessions can also contain these optional attributes:
et-no-initial-text et-initial-text
and 0 to n pairs of
et-filter=filt et-to=filt-to
these have to appear immediately following the et-prompt, with
et-filter followed immediately by et-to all these attributes have to
appear after the two mandatory attributes

this means when there are multiple attributes that this system
recognises, they all have to be next to each other, not have non
recognised attributes interspersed. if there are matching attributes
which aren't next to each other, this gives an error

what's the best way to parse this?
could try to maximum the logic in the implicit parsing grammar,
or parse permissively then do a second parse

I think definitely parse the header line
check the { }
and parse a list of attributes

then process the list of attributes to see if it's recognised, and if
so, if it's valid

-}

nonnewlinewhitespace :: Parser ()
nonnewlinewhitespace =
    void $ takeWhile1P (Just "whitespace") (\x -> isSpace x && x /= '\n')

lexeme :: Parser a -> Parser a
lexeme p = p <* void (optional nonnewlinewhitespace)

attribute :: Parser (Text, Maybe Text)
attribute =
    lexeme ((,)
        <$> (iden <?> "attributeName")
        <*> optional (char '=' *> ((iden <|> quoted <|> doubleQuoted) <?> "attribute value")))
  where
    iden = takeWhile1P (Just "iden char") unquotedChar
    between' c = between (char c) (char c)
    quoted = between' '\'' $ takeWhileP (Just "not ' or \n") (`notElem` ("'\n" :: [Char]))
    doubleQuoted = between' '"' $ takeWhileP (Just "not \" or \n") (`notElem` ("\"\n" :: [Char]))
    -- don't let any of the unquoted chars be { or }
    unquotedChar = (`notElem` ("\"'>/={} \t" :: [Char]))

-- returns the line including the newline at the end if there is one
-- (this is not very efficient, but good enough for now)
-- if matches any et- attributes, returns all the attributes parsed out as well
parseHeader :: Parser (Text, Maybe ValidatedHeader)
parseHeader =
    match $ do
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
                  [VHFilePrefix <$> namedValueAttribute "et-file-prefix"
                  ,VHFile <$> namedValueAttribute "et-file"
                  ,do
                   x <- namedAttribute "et-run"
                   case x of
                       Nothing -> pure VHRunInline
                       Just v -> pure $ VHRun v
                  ,vhSession
                  ,VHContinue <$ noValueAttribute "et-continue"]
             endingAttributes (Just vh)
            ,errorAttribute
            ,attribute *> startingAttributes
            ,pure Nothing]
    vhSession = do
        s <- namedValueAttribute "et-session"
        p <- namedValueAttribute "et-prompt"
        -- todo: parse optional bits
        pure $ VHSession $ SessionOptions (Just s) p Nothing []
    -- todo: match any recognised attribute and error
    errorAttribute = do
        o <- getOffset
        choice $
            map (\x -> namedAttribute x *>
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
            ]
    
    namedAttribute :: Text -> Parser (Maybe Text)
    namedAttribute nm =
        lexeme (chunk nm *> optional (char '=' *> attributeValue))
    namedValueAttribute :: Text -> Parser Text
    namedValueAttribute nm = do
        lexeme (chunk nm *> char '=' *> attributeValue)
    noValueAttribute :: Text -> Parser ()
    noValueAttribute nm =
        lexeme (chunk nm *> choice
          [do
           o <- getOffset
           char '=' *> region (setErrorOffset o) (fail ("attribute should not have value: " <> T.unpack nm))
          ,pure ()])
    endingAttributes :: Maybe ValidatedHeader -> Parser (Maybe ValidatedHeader)
    endingAttributes mvh = choice
        [errorAttribute
        ,lexeme attribute *> endingAttributes mvh
        ,pure mvh]
    attributeValue = (iden <|> quoted <|> doubleQuoted) <?> "attribute value"
    iden :: Parser Text
    iden = takeWhile1P (Just "iden char") unquotedChar
    between' c = between (char c) (char c)
    quoted = between' '\'' $ takeWhileP (Just "not ' or \n") (`notElem` ("'\n" :: [Char]))
    doubleQuoted = between' '"' $ takeWhileP (Just "not \" or \n") (`notElem` ("\"\n" :: [Char]))
    -- don't let any of the unquoted chars be { or }
    unquotedChar = (`notElem` ("\"'>/={} \t" :: [Char]))

{-
parsing bodies
--------------
the body parser depends on the header
a body always ends at the first ~~~~ starting at the first char on a line

simple bodies
-> parse everything following the header to the ~~~~ as a single string

prompt bodies
parse single lines beginning with the given prompt as prompts
it will parse a prompt immediately following another prompt
parse the lines inbetween these as replies. replies always have
all the text between the starting delimiter or preceding prompt line,
and the ending delimiter or following prompt line, as a single string/reply
  containing newlines if needed

inline command bodies
this is a first line with "$ " then the command line
followed by the rest of the body as a string

inline command + prompt bodies
this is a first line with "$ " then the command line
followed by the prompt body syntax -> prompts and replies

-}

-- parse a mix of prompts and replies until ~~~~, if hit eof before
-- this, give an error
parsePromptBody :: Text -> Parser Int
parsePromptBody _prompt = undefined

-- parse the starting inline command, then a prompt body
parseInlineCmdPromptBody :: Text -> Parser Int
parseInlineCmdPromptBody _prompt = undefined
