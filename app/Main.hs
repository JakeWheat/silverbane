{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Prelude as Pr
import Prelude hiding (error, show, putStrLn)

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import qualified Data.Text.IO as T

import qualified ExpectTest as E
import System.Environment (getArgs)
import GHC.Stack (HasCallStack)

main :: IO ()
main = do

    as <- getArgs
    let fn = case as of
                 [x] -> x
                 _ -> error $ "please pass single filename to process, got " <> T.pack (unwords as)

    input <- T.readFile fn
    es <- E.expectTest fn input
    flip mapM_ es $ \e -> putStrLn $ E.prettyExpectError e

-- todo: add callstack
error :: HasCallStack => Text -> a
error = Pr.error . T.unpack

show :: Show a => a -> Text
show = T.pack . Pr.show
