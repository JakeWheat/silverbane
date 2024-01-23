
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestExpectTest (expectTestTests) where

import qualified Text.RawString.QQ as R

import Test.Hspec
    (SpecWith
    ,it
    ,describe
    )

import ExpectTest as E
import qualified Data.Text as T
import Data.Text (Text)
import Utils

expectTestTests :: SpecWith ()
expectTestTests = describe "expect tests" $ do
    mapM_ (\(a,b,c) -> makeExpectTestTest a b c) expectTestExamples

makeExpectTestTest :: Text -> Text -> [Text] -> SpecWith ()
makeExpectTestTest nm src tgt =
    it (T.unpack nm) $ expectTest (T.unpack nm) src `expectErrorsShouldMatch` tgt

expectTestExamples :: [(Text,Text,[Text])]
expectTestExamples =
    [("file", [R.r|

~~~~{et-file='testfiles/testfile'}
testfilecontent
~~~~

|], [])
    ,("file-wrong-content", [R.r|

~~~~{et-file='testfiles/testfile'}
wrong testfilecontent
~~~~

|], ["file-wrong-content:3:0: files don't match"])
    -- todo: better way to check for text in the error, and separately check
    -- the position
    -- idea: instead of doing this, have a single golden test of a large document
    -- which will test all the errors, then you can update this one test
    -- relatively easily if e.g. the formatting is changed, and it will
    -- also test the line numbers

    ,("file-doesn-t-exist", [R.r|

~~~~{et-file='testfiles/noexist'}
testfilecontent
~~~~

|], ["file not found: testfiles/noexist"])

    ,("testfiles/file-path-relative", [R.r|

~~~~{et-file='testfile'}
testfilecontent
~~~~

|], [])

    -- regular runs
    -- check current directory
    
    -- python interaction


    -- final prompt being insignificant

    -- insignificant whitespace in sessions

    -- continue + switching sessions

    -- failure in continue

    -- no initial text option

    -- filters
    
    ]

