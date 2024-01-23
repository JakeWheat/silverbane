
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

-- avoid testing syntax variations that are handled in the parser tests

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

    ,("simple run", [R.r|
~~~~{et-run='echo stuff'}
stuff
~~~~
|], [])

-- todo: come back to this
    
{-    ,("check relative dir", [R.r|
~~~~{et-run='cat testfile; ls -l testfile'}
stuff
~~~~
|], [])-}

    -- TODO: check run fails
    
    ,("simple session 1", [R.r|
~~~~{et-session='python3' et-prompt='>>> '}
Python 3.11.2 (main, Mar 13 2023, 12:18:29) [GCC 12.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
~~~~
|], [])

    ,("simple session 1", [R.r|
~~~~{et-session='python3' et-prompt='>>> '}

Python 3.11.2 (main, Mar 13 2023, 12:18:29) [GCC 12.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.

>>> 
~~~~
|], [])

    
    ,("simple session 1.5", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> 
~~~~
|], [])

    
    ,("simple session 2", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
~~~~
|], [])

        ,("simple session 3", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}

>>> 

~~~~
|], [])

        ,("simple session 4", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> 1 + 2
3
~~~~
|], [])

        ,("simple session 4.5", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> print("line1\nline2\nline3")
line1
line2
line3
~~~~
|], [])

        
        ,("simple session 4", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> 1 + 2
3
>>> 
~~~~
|], [])

        ,("simple session 5", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> 1 + 2
3
>>> 3 + 4

7

>>> print('hello')
hello
~~~~
|], [])

        ,("simple session fail 1", [R.r|
~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> 1 + 2
4
~~~~
|], ["output doesn't match"])

    -- todo: check relative dir in sessions

        ,("session continue", [R.r|
~~~~{et-session et-prompt=">>> " et-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

~~~~{et-continue}
>>> 3 + 4
7
~~~~
|], [])
        
        ,("session continue switch", [R.r|
~~~~{et-session='ghci' et-prompt='ghci> ' et-no-initial-text}
ghci> 1 + 2
3
ghci> 3 + 5
8
ghci> 3 + 7
10
ghci> 
~~~~

check this is definitely ghci and not python

~~~~{et-continue}
ghci> (3 :: Int) + 4
7
~~~~

~~~~{et-session et-prompt=">>> " et-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

check this is definitely python and not ghci

~~~~{et-continue}
>>> 3 + 4 if isinstance(3, int) else None
7
~~~~
|], [])

        ,("double check session continue switch", [R.r|
~~~~{et-session='ghci' et-prompt='ghci> ' et-no-initial-text}
ghci> 1 + 2
3
ghci> 3 + 5
8
ghci> 3 + 7
10
ghci> 
~~~~

~~~~{et-continue}
ghci> (3 :: Int) + 4
7
~~~~

~~~~{et-session et-prompt=">>> " et-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

check this is definitely python and not ghci

~~~~{et-continue}
>>> (3 :: Int) + 4
7
~~~~
|], ["SyntaxError: invalid syntax"])

        ,("mismatch in continue", [R.r|
~~~~{et-session et-prompt=">>> " et-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

~~~~{et-continue}
>>> 3 + 4
8
~~~~
|], ["output doesn't match"])


    -- session run fails

    -- session exiting unexpectedly in the middle

    -- session exiting unexpectedly in continue

    -- filters

    ]

