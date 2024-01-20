
{-# LANGUAGE OverloadedStrings #-}
module Pexpect
    (Pexpect
    ,initPexpect
    ,spawn
    ,sendline
    ,expect
    ,close
    ) where


import qualified PythonWrapper as Py
import Data.Text (Text)
import Control.Monad (void)

data Pexpect = Pexpect Py.PyObject

initPexpect :: IO ()
initPexpect = do
    Py.initialize
    void $ either (error . show) id <$> Py.script "import pexpect"
spawn :: Text -> IO Pexpect
spawn cmd =  do
    args <- sequence [Py.toPyObject cmd]
    fn <- either (error . show) id <$> Py.eval "lambda x: pexpect.spawn(x, encoding='utf=8')"
    Pexpect <$> either (error . show) id <$> Py.app fn args

sendline :: Pexpect -> Text -> IO ()
sendline (Pexpect p) l = do
    args <- sequence [Py.toPyObject l]
    sl <- either (error . show) id <$> Py.getAttr p "sendline"
    void $ either (error . show) id <$> Py.app sl args

expect :: Pexpect -> Text -> IO Text
expect (Pexpect p) prompt = do
    fn <- either (error . show) id <$> Py.getAttr p "expect"
    args <- sequence [Py.toPyObject prompt]
    void $ either (error . show) id <$> Py.app fn args
    bef <- either (error . show) id <$> Py.getAttr p "before"
    either (error . show) id <$> Py.fromPyObject bef

close :: Pexpect -> IO (Int,Maybe Int)
close (Pexpect p) = do
    fn <- either (error . show) id <$> Py.getAttr p "close"
    void $ either (error . show) id <$> Py.app fn []
    v1 <- either (error . show) id <$> Py.getAttr p "exitstatus"
    v2 <- either (error . show) id <$> Py.getAttr p "signalstatus"
    v1' <- either (error . show) id <$> Py.fromPyObject v1
    t <- Py.isPyNone v2
    v2' <- if t
           then pure Nothing
           else either (error . show) Just <$> Py.fromPyObject v2
    pure (v1', v2')
