{-

TODO:

use a mock for pexpect for most of this testing, it will be much much
faster and allow obvious direct testing of all the variations

then do a trivial end to end test

and see if there's enough direct pexpect tests

-}
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

        ,("simple session 6", [R.r|
check variations on prompts which don't output anything

~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}
>>> import string
>>> string.capwords('test')
'Test'
~~~~

~~~~{et-session='python3' et-prompt='>>> ' et-no-initial-text}

>>> import string

>>> string.capwords('test2')

'Test2'

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

        ,("filter", [R.r|

The point of filter is if you have a small amount of variability in
the output, and you want to ignore that isolated bit. Inspired by
checking the output of hspec, which includes a random seed which
changes when there's a failure, and also includes the time the
tests took.

Currently uses regexes. This could be extended if it's too limiting.

It replaces the filter regex matches with the to text before comparing.
It does this with both the document text and the text coming from
running the spawnee. This isn't ideal when you want to write a prompt,
then run this program, get an error, and then paste the correct text into
your document, which is a way of working that should be supported.

~~~~{et-session et-prompt=">>> " et-no-initial-text et-filter="my random = 0\.[0-9]+" et-to='my random = 0.42353301245135155'}
$ python3
>>> import random
>>> print(f"This is an example, my random = {random.random()}, more stuff")
This is an example, my random = 0.12341234123412345, more stuff
~~~~

|], [])

        ,("check filter stays in its lane", [R.r|
~~~~{et-session et-prompt=">>> " et-no-initial-text et-filter="my random = 0\.[0-9]+" et-to='my random = 0.42353301245135155'}
$ python3
>>> import random
>>> print(f"This is an example, my random = {random.random()}, more stuff")
This is an example, my random = 0.12341234123412345, more stuff1
~~~~

|], ["output doesn't match"])


    ]

