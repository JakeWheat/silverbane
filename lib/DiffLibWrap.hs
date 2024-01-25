{-

Wrap a call to python difflib - so can display a human readable diff
when two chunks of text don't match

TODO: maybe tune it if the expected text is empty or only whitespace

-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module DiffLibWrap
    (initDiffLibWrap
    ,doDiff
    ,DiffSpec(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified PythonWrapper as Py
import GHC.Stack (HasCallStack)
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)

initDiffLibWrap :: IO ()
initDiffLibWrap = do
    Py.initialize
    void $ e <$> Py.script "import difflib"

data DiffSpec
    = D
    {fromName :: Text
    ,fromText :: Text
    ,toName :: Text
    ,toText :: Text}

doDiff :: DiffSpec -> Text
doDiff D {..} = unsafePerformIO $ do
    args <- sequence [Py.toPyObject fromText
                     ,Py.toPyObject toText
                     --,Py.toPyObject fromName
                     --,Py.toPyObject toName
                     ]
    -- looks better without the extra lines, you lose the line numbers
    -- but you're supposed to be eyeballing the diff on small strings
    -- fn <- e <$> Py.eval "lambda f, t, fn, tn: '\\n'.join(difflib.unified_diff(f.splitlines(), t.splitlines(), fromfile=fn, tofile=tn))"
    fn <- e <$> Py.eval "lambda f, t: '\\n'.join(difflib.unified_diff(f.splitlines(), t.splitlines(), lineterm=''))"
    a <-e <$> Py.app fn args
    res <- e <$> Py.fromPyObject a
    -- drop the boilerplate
    -- todo: this would be more robust if it checked these dropped lines look how
    -- they are expected to look, that they start with ---,+++,@@ respectively
    pure . T.unlines . drop 3 . T.lines $ res


e :: HasCallStack => Either Py.PythonError a -> a
e a = either (error . show) id a
