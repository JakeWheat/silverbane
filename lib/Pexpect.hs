{-

Project specific wrapper for pexpect. It's ugly, and has a bunch of
problems which aren't important for this project (such as accurate
leading and trailing whitespace handling).

To fix this:

option 1: fix python-wrapper
option 2: replace this code completely
  2a: another haskell python wrapper
  2b: implement an expect clone directly in haskell

-}

{-# LANGUAGE OverloadedStrings #-}
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

data Pexpect = Pexpect Py.PyObject

initPexpect :: IO ()
initPexpect = do
    Py.initialize
    void $ e <$> Py.script "import pexpect"

spawn :: Text -> IO Pexpect
spawn cmd =  do
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
    e <$> Py.fromPyObject bef

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

e :: Either Py.PythonError a -> a
e = either (error . show) id

exchange :: Pexpect -> Text -> Text -> IO Text
exchange p prompt sl = do
    sendline p sl
    rep <- expect p prompt
    pure $ if sl `T.isPrefixOf` rep
        then T.drop (T.length sl) rep
        else rep
