
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
    describe "headerbits" $ mapM_ (uncurry makeValidHeaderTest) validHeaders
    describe "headerbits errors" $ mapM_ (uncurry makeErrorHeaderTest) errorHeaders

{-

headerBits:

regular line
line with ~~~~, no atts
line with ~~~~, no recognised attributes
  do each kind of attribute
  a
  #a
  .a
  a=b
  a='b'
  a="b"

error with missing trailing }
error with unclosed ", '
-}

makeValidHeaderTest :: Text -> (Text, Maybe ([(Text, Maybe Text)])) -> SpecWith ()
makeValidHeaderTest input tgt = 
    it ("header: " <> T.unpack input) $ parse (headerBits <* eof) "" input `shouldParse` tgt

makeErrorHeaderTest :: Text -> Text -> SpecWith ()
makeErrorHeaderTest input e = 
    it ("header error: " <> T.unpack input)
        $ parse (headerBits <* eof) "" input
          `shouldFailContains` e

validHeaders :: [(Text, (Text, Maybe ([(Text, Maybe Text)])))]
validHeaders =
    [("~~~~", ("~~~~", Nothing))
    ,("~~~~{}", ("~~~~{}", Just []))
    ,("~~~~{a}", ("~~~~{a}", Just [("a", Nothing)]))
    ,("~~~~ { a } ", ("~~~~ { a } ", Just [("a", Nothing)]))
    ,("~~~~{ a=b }", ("~~~~{ a=b }", Just [("a", Just "b")]))
    ,("~~~~{ a='b' }", ("~~~~{ a='b' }", Just [("a", Just "b")]))
    ,("~~~~{ a=\"b\" }", ("~~~~{ a=\"b\" }", Just [("a", Just "b")]))
    ,("~~~~{ .sql #myclass et-f1 et-f2=stuff }"
     ,("~~~~{ .sql #myclass et-f1 et-f2=stuff }"
      ,Just [(".sql", Nothing)
            ,("#myclass", Nothing)
            ,("et-f1", Nothing)
            ,("et-f2", Just "stuff")]))
    ]

errorHeaders :: [(Text, Text)]
errorHeaders =
    [("~~~~{", "expecting '}'")
    ,("~~~~{a='", "expecting '''")
    ,("~~~~{a=\"", "expecting '\"'")
    ,("~~~~{{", "unexpected '{'")
    ,("~~~~{} a", "unexpected 'a'")]




{-


validate header:
et-file=
et=file-prefix=
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
