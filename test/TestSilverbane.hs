{-

TODO:

use a mock for pexpect for most of this testing, it will be much much
faster and allow obvious direct testing of all the variations

then do a trivial end to end test

and see if there's enough direct pexpect tests

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestSilverbane (silverbaneTests) where

import qualified Text.RawString.QQ as R

import Test.Hspec
    (SpecWith
    ,it
    ,describe
    )

import qualified Silverbane as S
import qualified Data.Text as T
import Data.Text (Text)
import Utils

silverbaneTests :: SpecWith ()
silverbaneTests = describe "silverbane tests" $ do
    mapM_ (\(a,b,c) -> makeSilverbaneTest a b c) silverbaneExamples

makeSilverbaneTest :: Text -> Text -> [Text] -> SpecWith ()
makeSilverbaneTest nm src tgt =
    it (T.unpack nm) $ S.checkDocument (T.unpack nm) src `silverbaneErrorsShouldMatch` tgt

-- avoid testing syntax variations that are handled in the parser tests

silverbaneExamples :: [(Text,Text,[Text])]
silverbaneExamples =
    [--------------------------------------
     --  file tests
     ("file", [R.r|

~~~~{sb-file='testfiles/testfile'}
testfilecontent
~~~~

|], [])
    ,("file-wrong-content", [R.r|

~~~~{sb-file='testfiles/testfile'}
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

~~~~{sb-file='testfiles/noexist'}
testfilecontent
~~~~

|], ["file not found: testfiles/noexist"])

    ,("testfiles/file-path-relative", [R.r|

~~~~{sb-file='testfile'}
testfilecontent
~~~~

|], [])

    --------------------------------------
    -- run process
    
    ,("simple run", [R.r|
~~~~{sb-run='echo stuff'}
stuff
~~~~
|], [])

    ,("starting run process fails", [R.r|
~~~~{sb-run='echo1 stuff'}
stuff
~~~~
|], ["echo1: not found"])

    
    ,("check interleaved stdout and err", [R.r|
~~~~{sb-run='testfiles/testscript.py 0'}
stdoutline1
stdoutline2
stderrline1
stderrline2
stdoutline3
stdoutline4
stderrline3
stderrline4
~~~~
|], [])

    ,("check expected error code", [R.r|
~~~~{sb-run='testfiles/testscript.py 1' sb-non-zero-exit}
stdoutline1
stdoutline2
stderrline1
stderrline2
stdoutline3
stdoutline4
stderrline3
stderrline4
~~~~
|], [])

    ,("check unexpected non zero error", [R.r|
~~~~{sb-run='testfiles/testscript.py 1'}
stdoutline1
stdoutline2
stderrline1
stderrline2
stdoutline3
stdoutline4
stderrline3
stderrline4
~~~~
|], ["process exited with non zero"])

        ,("check unexpected 0 exit code", [R.r|
~~~~{sb-run='testfiles/testscript.py 0' sb-non-zero-exit}
stdoutline1
stdoutline2
stderrline1
stderrline2
stdoutline3
stdoutline4
stderrline3
stderrline4
~~~~
|], ["process didn't exit with non zero"])


    ,("testfiles/check-relative-dir", [R.r|
~~~~{sb-run='cat testfile'}
testfilecontent
~~~~
|], [])

    -- check cwd

     
    ,("testfiles/check-relative-dir-should-fail", [R.r|
check it can't find this file any more
~~~~{sb-run='cat testfile' sb-cwd='..'}
testfilecontent
~~~~
|], ["testfile: No such file or directory"])

    -- test here that it doesn't work without cwd
    ,("check-relative-dir-doublecheck", [R.r|
~~~~{sb-run='cat testfile'}
testfilecontent
~~~~
|], ["testfile: No such file or directory"])

    
    ,("check-relative-dir-succeed", [R.r|
~~~~{sb-run='cat testfile' sb-cwd='testfiles/'}
testfilecontent
~~~~
|], [])

     
    ,("run with quoted args", [R.r|
~~~~{sb-run='testfiles/echoscript.py "this is one arg" "this is another"'}
this is one arg
this is another
~~~~
|], [])

    -- todo: add test for session with quoted args
    
    --------------------------------------
    -- simple sessions

    ,("simple session 1", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> '}
Python 3.11.2 (main, Mar 13 2023, 12:18:29) [GCC 12.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
~~~~
|], [])

    ,("simple session 1", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> '}

Python 3.11.2 (main, Mar 13 2023, 12:18:29) [GCC 12.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.

>>> 
~~~~
|], [])

    
    ,("simple session 1.5", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> 
~~~~
|], [])

    
    ,("simple session 2", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
~~~~
|], [])

        ,("simple session 3", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}

>>> 

~~~~
|], [])

        ,("simple session 4", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> 1 + 2
3
~~~~
|], [])

        ,("simple session 4.5", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> print("line1\nline2\nline3")
line1
line2
line3
~~~~
|], [])

        
        ,("simple session 4", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> 1 + 2
3
>>> 
~~~~
|], [])

        ,("simple session 5", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
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

~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> import string
>>> string.capwords('test')
'Test'
~~~~

~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}

>>> import string

>>> string.capwords('test2')

'Test2'

~~~~
|], [])

        ,("simple session fail 1", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> 1 + 2
4
~~~~
|], ["output doesn't match"])


        ,("testfiles/session-relative-dir", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> f = open('testfile')
>>> print(f.read())
testfilecontent
~~~~
|], [])


        ,("long line", [R.r|
~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-no-initial-text}
ghci> unwords ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]
"one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty"
~~~~
|], [])


   {- ,("session relative dir", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> '}
Python 3.11.2 (main, Mar 13 2023, 12:18:29) [GCC 12.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> 
~~~~
|], [])-}

    ,("testfiles/session-relative-dir-fail", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text sb-cwd='..'} 
>>> f = open('testfile')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
FileNotFoundError: [Errno 2] No such file or directory: 'testfile'
~~~~
|], [])

    -- test here that it doesn't work without cwd
    ,("session-relative-dir-fail", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> f = open('testfile')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
FileNotFoundError: [Errno 2] No such file or directory: 'testfile'
~~~~
|], [])

    ,("check-session-relative-dir-succeed", [R.r|
~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text sb-cwd='testfiles'}
>>> f = open('testfile')
>>> f.read()
'testfilecontent\n'
~~~~
|], [])

    ,("session with args pre sanity", [R.r|
~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-no-initial-text}
ghci> import Data.Text
ghci> "test" :: Text
<interactive>:2:1: error: [GHC-83865]
    • Couldn't match type ‘[Char]’ with ‘Text’
      Expected: Text
        Actual: String
    • In the expression: "test" :: Text
      In an equation for ‘it’: it = "test" :: Text
~~~~
|], [])

    ,("session with args", [R.r|
~~~~{sb-session='ghci -XOverloadedStrings' sb-prompt='ghci> ' sb-no-initial-text}
ghci> import Data.Text
ghci> "test" :: Text
"test"
~~~~
|], [])


    --------------------------------------
    -- session continue
        
    -- todo: check relative dir in sessions

        ,("session continue", [R.r|
~~~~{sb-session sb-prompt=">>> " sb-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

~~~~{sb-continue}
>>> 3 + 4
7
~~~~
|], [])
        
        ,("session continue switch", [R.r|
~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-no-initial-text}
ghci> 1 + 2
3
ghci> 3 + 5
8
ghci> 3 + 7
10
ghci> 
~~~~

check this is definitely ghci and not python

~~~~{sb-continue}
ghci> (3 :: Int) + 4
7
~~~~

~~~~{sb-session sb-prompt=">>> " sb-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

check this is definitely python and not ghci

~~~~{sb-continue}
>>> 3 + 4 if isinstance(3, int) else None
7
~~~~
|], [])

        ,("double check session continue switch", [R.r|
~~~~{sb-session='ghci' sb-prompt='ghci> ' sb-no-initial-text}
ghci> 1 + 2
3
ghci> 3 + 5
8
ghci> 3 + 7
10
ghci> 
~~~~

~~~~{sb-continue}
ghci> (3 :: Int) + 4
7
~~~~

~~~~{sb-session sb-prompt=">>> " sb-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

check this is definitely python and not ghci

~~~~{sb-continue}
>>> (3 :: Int) + 4
7
~~~~
|], ["SyntaxError: invalid syntax"])

        ,("mismatch in continue", [R.r|
~~~~{sb-session sb-prompt=">>> " sb-no-initial-text}
$ python3
>>> 1 + 2
3
~~~~

~~~~{sb-continue}
>>> 3 + 4
8
~~~~
|], ["output doesn't match"])


    --------------------------------------
    -- session assorted anomalies
        
        ,("session run fails", [R.r|
~~~~{sb-session='wython3' sb-prompt='>>> '}
>>> print("line1\nline2\nline3")
line1
line2
line3
~~~~
|], ["The command was not found or was not executable: wython3."])

        -- todo: catch this specific situation and give a better error message
        ,("continue after session run fails", [R.r|
~~~~{sb-session='wython3' sb-prompt='>>> '}
>>> print("line1\nline2\nline3")
line1
line2
line3
~~~~

~~~~{sb-continue}
>>> 3 + 4
8
~~~~

|], ["The command was not found or was not executable: wython3.", "continue without previous session" ])

    -- session exiting unexpectedly in the middle

    -- session exiting unexpectedly in continue

    -- at the moment, you'll catch both the above because the output
    -- won't match, but it will be confusing unless you can guess
    -- that it's because the process exited early

    --------------------------------------
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

~~~~{sb-session sb-prompt=">>> " sb-no-initial-text sb-filter="my random = 0\.[0-9]+" sb-to='my random = 0.42353301245135155'}
$ python3
>>> import random
>>> print(f"This is an example, my random = {random.random()}, more stuff")
This is an example, my random = 0.12341234123412345, more stuff
~~~~

|], [])

        ,("check filter stays in its lane", [R.r|
~~~~{sb-session sb-prompt=">>> " sb-no-initial-text sb-filter="my random = 0\.[0-9]+" sb-to='my random = 0.42353301245135155'}
$ python3
>>> import random
>>> print(f"This is an example, my random = {random.random()}, more stuff")
This is an example, my random = 0.12341234123412345, more stuff1
~~~~

|], ["output doesn't match"])


    ]

