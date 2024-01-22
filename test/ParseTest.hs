
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ParseTest (parseTests) where

import Test.Hspec
    (SpecWith
    ,it
    ,describe
    )

import Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec (parse, eof)

import Parse as P

import qualified Data.Text as T
import Data.Text (Text)
import TestUtils (shouldFailContains)

parseTests :: SpecWith ()
parseTests = describe "parse" $ do
    describe "header errors" $ mapM_ (uncurry makeErrorHeaderTest) errorHeaders
    describe "validate header" $ do
        testOKHeader
        mapM_ (uncurry makeValidatedHeaderTest) validatedHeaders

makeErrorHeaderTest :: Text -> Text -> SpecWith ()
makeErrorHeaderTest input e = 
    it ("header error: " <> T.unpack input)
        $ parse (parseHeader <* eof) "" input
          `shouldFailContains` e

errorHeaders :: [(Text, Text)]
errorHeaders =
    [("~~~~{\n", "unexpected")
    ,("~~~~{a='\n", "expecting '''")
    ,("~~~~{a=\"\n", "expecting '\"'")
    ,("~~~~{{\n", "unexpected '{'")
    ,("~~~~{} a\n", "unexpected 'a'")
    
    ,("~~~~{et-file=x et-file=x}\n", "unexpected et-file")
    ,("~~~~{et-session=x stuff}\n", "unexpected \"stuff")
    ,("~~~~{et-session=x}\n", "expecting \"et-prompt\"")
    ,("~~~~{et-continue=x}\n", "attribute should not have value")
    ]

{-


validate header:
et-file=
et-file-prefix=
et-run=
et-run
et-session= et-prompt=
et-continue
  I think this needs a state transformer, booo, but so be it

et-no-initial-text
et-initial-text
et-filter="" et-to=""
+ multiple

 -}

makeValidatedHeaderTest :: Text -> ValidatedHeader -> SpecWith ()
makeValidatedHeaderTest input tgt = 
    it ("validated header: " <> T.unpack input) $ parse parseHeader "" input `shouldParse` (input,Just tgt)

testOKHeader :: SpecWith ()
testOKHeader =
    it ("OK validated header") $
    let input = "~~~~{a=b c d}"
    in parse parseHeader "" input
       `shouldParse` (input,Nothing)

validatedHeaders :: [(Text, ValidatedHeader)]
validatedHeaders =
    [("~~~~{et-file=filename}", VHFile "filename")
    ,("~~~~{et-file-prefix='--'}", VHFilePrefix "--")
    ,("~~~~{et-run='echo stuff'}", VHRun "echo stuff")
    ,("~~~~{et-run}", VHRunInline)
    ,("~~~~{et-session='ghci' et-prompt='ghci> '}"
     ,VHSession (SessionOptions (Just "ghci") "ghci> " Nothing []))
     -- todo: inline
    ,("~~~~{et-continue}", VHContinue)

    ,("~~~~{.sql et-file=filename}", VHFile "filename")
    ,("~~~~{.sql et-file=filename #myclass stuff}", VHFile "filename")
    ]
     -- todo: session options

-- todo: validate header parse errors -> wrong attributes
