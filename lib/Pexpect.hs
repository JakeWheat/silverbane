{-

Project specific wrapper for pexpect. It's ugly, and has a bunch of
questionable bits which aren't important for this project (such as
accurate leading and trailing whitespace handling, and automatically
removing echo'd sendlines, not even sure why this is happening).

To fix this:

option 1: fix python-wrapper
option 2: replace this code completely
  2a: another haskell python wrapper
  2b: implement an expect clone directly in haskell

and figure out why sendlines are sometimes echo'd (I think it doesn't
do it with python, but does with ghci?)

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Pexpect
    (Pexpect
    ,initPexpect
    ,spawn
    ,sendline
    ,expect
    ,close
    ,exchange
    ) where

import qualified PythonWrapper as Py
import Data.Text (Text)
import Control.Monad (void)

import qualified Data.Text as T
import GHC.Stack (HasCallStack)

data Pexpect = Pexpect Py.PyObject


initPexpect :: IO ()
initPexpect = do
    Py.initialize
    void $ e <$> Py.script "import pexpect"

spawn :: Maybe Text -> Text -> IO Pexpect
spawn (Just cwd) cmd =  do
    args <- sequence [Py.toPyObject cmd, Py.toPyObject cwd]
    --putStrLn $ "spawn: " <> T.unpack cmd
    fn <- e <$> Py.eval "lambda x, y: pexpect.spawn(x, encoding='utf=8', cwd=y, echo=False)"
    Pexpect <$> e <$> Py.app fn args
spawn Nothing cmd =  do
    args <- sequence [Py.toPyObject cmd]
    fn <- e <$> Py.eval "lambda x: pexpect.spawn(x, encoding='utf=8')"
    Pexpect <$> e <$> Py.app fn args

sendline :: Pexpect -> Text -> IO ()
sendline (Pexpect p) l = do
    args <- sequence [Py.toPyObject l]
    sl <- e <$> Py.getAttr p "sendline"
    void $ e <$> Py.app sl args

expect :: Pexpect -> Text -> IO Text
expect (Pexpect p) prompt = do
    fn <- e <$> Py.getAttr p "expect"
    args <- sequence [Py.toPyObject prompt]
    void $ e <$> Py.app fn args
    bef <- e <$> Py.getAttr p "before"
    T.filter (/= '\r') . e <$> Py.fromPyObject bef

close :: Pexpect -> IO (Int,Maybe Int)
close (Pexpect p) = do
    fn <- e <$> Py.getAttr p "close"
    void $ e <$> Py.app fn []
    v1 <- e <$> Py.getAttr p "exitstatus"
    v2 <- e <$> Py.getAttr p "signalstatus"
    v1' <- e <$> Py.fromPyObject v1
    t <- Py.isPyNone v2
    v2' <- if t
           then pure Nothing
           else Just . e <$> Py.fromPyObject v2
    pure (v1', v2')


e :: HasCallStack => Either Py.PythonError a -> a
e !a = either (error . show) id a

exchange :: Pexpect -> Text -> Text -> IO Text
exchange p prompt sl = do
    sendline p sl
    rep <- expect p prompt
    -- not sure how you'd develop either this program itself
    -- or with this program on both windows and linux robustly
    -- so linux line endings only for now
    let rep1 = T.filter (/= '\r') rep
    -- unbelievable hacky, this is not very robust
    -- python doesn't seem to need it, but ghci does, not sure why
    -- should get to the bottom of it
    pure $ if T.stripEnd sl `T.isPrefixOf` T.stripStart rep1
        then T.drop (T.length $ T.stripEnd sl) $ T.stripStart rep1
        else rep1
