{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

import qualified Silverbane as S
import System.Environment (getArgs)
import GHC.Stack (HasCallStack)

import System.Exit (exitFailure)
import Control.Monad (unless)

main :: IO ()
main = do
    as <- getArgs
    let fn = case as of
                 [x] -> x
                 _ -> errorT $ "please pass single filename to process, got " <> T.pack (unwords as)
    input <- T.readFile fn
    es <- S.checkDocument fn input
    flip mapM_ es $ \e -> T.putStrLn $ S.prettyError e
    unless (null es) $ exitFailure

errorT :: HasCallStack => Text -> a
errorT = error . T.unpack

showT :: Show a => a -> Text
showT = T.pack . show
