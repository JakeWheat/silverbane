
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
    pure . T.unlines . drop 3 . T.lines $ res


e :: HasCallStack => Either Py.PythonError a -> a
e a = either (error . show) id a
