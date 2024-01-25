
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module TestParse (parseTests) where

import Test.Hspec
    (SpecWith
    ,it
    ,describe
    ,HasCallStack
    )

import Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec (eof)

import qualified Text.RawString.QQ as R
import Parse as P

import qualified Data.Text as T
import Data.Text (Text)
import Utils (shouldFailContains)

import Assertion
    (FileChunk(..)
    ,EtFile(..)
    ,EtRun(..)
    ,EtSession(..)
    ,EtContinue(..)
    ,SessionLine(..)
    )

parseTests :: SpecWith ()
parseTests = describe "parse" $ do
    describe "header errors" $ mapM_ (uncurry makeErrorHeaderTest) errorHeaders
    describe "validate header" $ do
        testOKHeader
        mapM_ (uncurry makeValidatedHeaderTest) validatedHeaders
    describe "body" $ do
        mapM_ (\(a,b,c) -> makeBodyTest a b c) bodies
        mapM_ (\(a,b,c,d) -> makeInlineBodyTest a b (c,d)) inlineBodies
    describe "files" $ do
        mapM_ (uncurry makeFileTest) simpleFiles
       
    --describe "file errors" $ do
    --    mapM_ (uncurry makeFileErrorTest) simpleFileErrors

makeErrorHeaderTest :: HasCallStack => Text -> Text -> SpecWith ()
makeErrorHeaderTest input e = 
    it ("header error: " <> quickShow input)
        $ myRunParse (header <* eof) "" input
          `shouldFailContains` e

-- probably needs some more work to disambiguate test names better
-- for the tests with long inputs, add an explicit name to the
-- examples
quickShow :: Text -> String
quickShow t = T.unpack $ T.map replaceNewline $ T.take 40 t
  where
    replaceNewline '\n' = ' '
    replaceNewline x = x

errorHeaders :: [(Text, Text)]
errorHeaders =
    [("~~~~{\n", "unexpected")
    ,("~~~~{a='\n", "expecting '''")
    ,("~~~~{a=\"\n", "expecting '\"'")
    ,("~~~~{{\n", "unexpected '{'")
    ,("~~~~{} a\n", "unexpected 'a'")
    
    ,("~~~~{sb-file=x sb-file=x}\n", "unexpected sb-file")
    ,("~~~~{sb-session=x stuff}\n", "unexpected \"stuff")
    ,("~~~~{sb-session=x}\n", "expecting \"sb-prompt\"")
    ,("~~~~{sb-continue=x}\n", "attribute should not have value")
    -- todo: sb-to without sb-filter, with attribute in between
    ]

{-


validate header:
sb-file=
sb-file-prefix=
sb-run=
sb-run
sb-session= sb-prompt=
sb-continue
  I think this needs a state transformer, booo, but so be it

sb-no-initial-text
sb-initial-text
sb-filter="" sb-to=""
+ multiple

 -}

makeValidatedHeaderTest :: HasCallStack => Text -> ValidatedHeader -> SpecWith ()
makeValidatedHeaderTest input tgt = 
    it ("validated header: " <> quickShow input) $ myRunParse (header <* eof) "" input `shouldParse` Just tgt

testOKHeader :: SpecWith ()
testOKHeader =
    it ("OK validated header") $
    myRunParse (header <* eof) "" "~~~~{a=b c d}"
       `shouldParse` Nothing

validatedHeaders :: [(Text, ValidatedHeader)]
validatedHeaders =
    [("~~~~{sb-file=filename}", VHFile "filename")
    ,("~~~~{sb-file-prefix='--'}", VHFilePrefix "--")

    ,("~~~~{sb-run='echo stuff'}", VHRun Nothing "echo stuff" True)
    ,("~~~~{sb-run}", VHRunInline Nothing True)

    ,("~~~~{sb-run='echo stuff' sb-non-zero-exit}", VHRun Nothing "echo stuff" False)
    ,("~~~~{sb-run sb-non-zero-exit}", VHRunInline Nothing False)
    
    ,("~~~~{sb-run='echo stuff' sb-cwd='mydir'}", VHRun (Just "mydir") "echo stuff" True)
    ,("~~~~{sb-run sb-cwd='mydir'}", VHRunInline (Just "mydir") True)


    ,("~~~~{sb-session='ghci' sb-prompt='ghci> '}"
     ,VHSession (SessionOptions Nothing (Just "ghci") "ghci> " Nothing []))
    ,("~~~~{sb-session sb-prompt='ghci> '}"
     ,VHSession (SessionOptions Nothing Nothing "ghci> " Nothing []))

    ,("~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-no-initial-text}"
     ,VHSession (SessionOptions Nothing (Just "ghci") "ghci> " (Just False) []))

    ,("~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-filter='from' sb-to='to'}"
     ,VHSession (SessionOptions Nothing (Just "ghci") "ghci> " Nothing [("from", "to")]))

    ,("~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-filter='from' sb-to='to' sb-filter='alsofrom' sb-to='alsoto'}"
     ,VHSession (SessionOptions Nothing (Just "ghci") "ghci> " Nothing [("from", "to"), ("alsofrom", "alsoto")]))

    ,("~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-no-initial-text sb-filter='from' sb-to='to'}"
     ,VHSession (SessionOptions Nothing (Just "ghci") "ghci> " (Just False) [("from", "to")]))

    ,("~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-filter='from' sb-to='to' sb-no-initial-text}"
     ,VHSession (SessionOptions Nothing (Just "ghci") "ghci> " (Just False) [("from", "to")]))

    ,("~~~~{sb-session sb-prompt='ghci> '}"
     ,VHSession (SessionOptions Nothing Nothing "ghci> " Nothing []))

    
    ,("~~~~{sb-continue}", VHContinue)

    -- pandoc won't parse without the equals, so allow this as
    -- alternative no value attribute syntax (it was probably
    -- parsing to the wrong thing before also)
    ,("~~~~{sb-continue=}", VHContinue)

    ,("~~~~{.sql sb-file=filename}", VHFile "filename")
    ,("~~~~{.sql sb-file=filename #myclass stuff}", VHFile "filename")
    ]
     -- todo: session options

-- todo: validate header parse errors -> wrong attributes

makeBodyTest :: HasCallStack => Text -> Text -> [SessionLine] -> SpecWith ()
makeBodyTest prompt input tgt = 
    it ("body: " <> quickShow input) $ myRunParse (sessionBody prompt <* eof) "" input `shouldParse` tgt

bodies :: [(Text, Text, [SessionLine])]
bodies =
    [("ghci> ", [R.r|test
~~~~|], [Reply "test\n"])
    ,("ghci> ", [R.r|test
~~~~
|], [Reply "test\n"])

      -- add trailing newline
      -- only a prompt
    ,("ghci> ", [R.r|ghci> 
~~~~
|], [Prompt "\n"])
      -- prompt reply prompt
    ,("ghci> ", [R.r|ghci> 1 + 2
3
ghci> 
~~~~
|], [Prompt "1 + 2\n", Reply "3\n", Prompt "\n"])
      -- reply prompt reply
    ,("ghci> ", [R.r|pre stuff
more
ghci> 1 + 2
3
~~~~
|], [Reply "pre stuff\nmore\n", Prompt "1 + 2\n", Reply "3\n"])
    ]

makeInlineBodyTest :: HasCallStack => Text -> Text -> (Text, [SessionLine]) -> SpecWith ()
makeInlineBodyTest prompt input tgt = 
    it ("body: " <> quickShow input) $ myRunParse (inlineCmdSessionBody prompt <* eof) "" input `shouldParse` tgt

inlineBodies :: [(Text, Text, Text, [SessionLine])]
inlineBodies =
    [("ghci> ", [R.r|$ ghci
~~~~|], "ghci\n", [])
    ,("ghci> ", [R.r|$ ghci

~~~~|], "ghci\n", [Reply "\n"])
    ,("ghci> ", [R.r|$ ghci
ghci> 
~~~~|], "ghci\n", [Prompt "\n"])

    ]

makeFileTest :: HasCallStack => Text -> [FileChunk] -> SpecWith ()
makeFileTest input tgt = 
    it ("file: " <> quickShow input) $ myRunParse (file <* eof) "" input `shouldParse` tgt

simpleFiles :: [(Text, [FileChunk])]
simpleFiles =
    [("", [])
    ,("stuff", [])
    ,("\nstuff\n", [])
    
    ,([R.r|~~~~{sb-file=myfile}
stuff
stuff
~~~~
|], [FcFile (EtFile 1 "myfile" [R.r|stuff
stuff
|])])
    
    ,([R.r|~~~~{sb-file=myfile}
stuff
stuff
~~~~|], [FcFile (EtFile 1 "myfile" [R.r|stuff
stuff
|])])

    ,([R.r|~~~~{sb-file=myfile other-stuff}
stuff
stuff
~~~~|], [FcFile (EtFile 1 "myfile" [R.r|stuff
stuff
|])])

    ,([R.r|

preamble text

~~~~{sb-file=myfile}
stuff
stuff
~~~~

postamble text

|], [FcFile (EtFile 5 "myfile" [R.r|stuff
stuff
|])])

        ,([R.r|

preamble text

~~~~{sb-file=myfile}
block1
~~~~

~~~~{sb-file=myfile2}
block2
~~~~

postamble text

|], [FcFile (EtFile 5 "myfile" "block1\n")
    ,FcFile (EtFile 9 "myfile2" "block2\n")
    ])


-- example of each of the other blocks
-- file with inline filename
    ,([R.r|

~~~~{sb-file-prefix='--'}
-- File myfile
stuff
~~~~

~~~~{sb-file-prefix='--'}

-- File myfile1
stuff1
~~~~

~~~~{sb-file-prefix='--'}

-- myfile2
stuff2
~~~~


|], [FcFile (EtFile 3 "myfile" "stuff\n")
    ,FcFile (EtFile 8 "myfile1" "stuff1\n")
    ,FcFile (EtFile 14 "myfile2" "stuff2\n")
    ])

    ,([R.r|

~~~~{sb-run='echo stuff'}
stuff
~~~~
|], [FcRun (EtRun 3 Nothing "echo stuff" True "stuff\n")])

    ,([R.r|
~~~~{sb-run}
$ echo stuff
stuff
~~~~
|], [FcRun (EtRun 2 Nothing "echo stuff" True "stuff\n")])

    ,([R.r|
~~~~{sb-session='ghci' sb-prompt='ghci> '}
stuff
ghci> 1 + 2
3
~~~~

~~~~{sb-continue}
ghci> 3 + 4
7
~~~~

~~~~{sb-session sb-prompt=">>> "}
$ python3
>>> 1 + 2
3
ghci> 
~~~~

~~~~{sb-continue}
>>> 3 + 4
7
ghci> 
~~~~

~~~~{sb-session='stuff' sb-prompt=">>> " sb-no-initial-text sb-filter=f sb-to=t}
~~~~


|], [FcSession $ EtSession 2 Nothing "ghci" "ghci> " Nothing []
     [Reply "stuff\n"
     ,Prompt "1 + 2\n"
     ,Reply "3\n"]
    ,FcContinue $ EtContinue 8
     [Prompt "3 + 4\n"
     ,Reply "7\n"]
    ,FcSession $ EtSession 13 Nothing "python3" ">>> " Nothing []
     [Prompt "1 + 2\n"
     ,Reply "3\nghci> \n"]
    ,FcContinue $ EtContinue 20
     [Prompt "3 + 4\n"
     ,Reply "7\nghci> \n"]
    ,FcSession $ EtSession 26 Nothing "stuff" ">>> " (Just False) [("f", "t")] []
     ])

    ]


-- errors:
-- continue without session

{-makeFileErrorTest :: HasCallStack => Text -> Text -> SpecWith ()
makeFileErrorTest input e = 
    it ("file error: " <> quickShow input)
        $ myRunParse (pure "" <* file <* eof) "" input
          `shouldFailContains` e-}

{-simpleFileErrors :: [(Text, Text)]
simpleFileErrors =
    [    ("~~~~{\na}", "xx")

        --("~~~~{sb-session='ghci' sb-prompt='ghci> '\n", "xx")
    ]-}
