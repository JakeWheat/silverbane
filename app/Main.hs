{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

import qualified Silverbane as S
import System.Environment (getArgs)
import GHC.Stack (HasCallStack)

import System.Exit (exitFailure)
import Control.Monad (unless)

import qualified RegexReplace as Re
import Parse as P

main :: IO ()
main = do
   
    as <- getArgs
    case as of
        [x] -> checkDoc x
        ["test-filters", filters, fn] -> testFilters filters fn
        _ -> do
            T.putStr $ T.unlines
                ["unsuppored args, got " <> T.pack (unwords as)
                ,"Usage:"
                ,"silverbane [filename]                             test file"
                ,"silverbane test-filters [filterspec] [filename]   run file through given filters"]
            exitFailure

checkDoc :: FilePath -> IO ()
checkDoc fn = do
    input <- T.readFile fn
    es <- S.checkDocument fn input
    flip mapM_ es $ \e -> T.putStrLn $ S.prettyError e
    unless (null es) $ exitFailure

testFilters :: String -> FilePath -> IO ()
testFilters filterspec fn = do
    let fs = either (errorT . P.prettyError) id $ P.parseFilters $ T.pack filterspec
        filterf = let mf (re,sub) = Re.substitute (Re.compile re) sub
                  in foldr (.) id $ map mf fs
    src <- T.readFile fn
    T.putStrLn $ filterf src

errorT :: HasCallStack => Text -> a
errorT = error . T.unpack

showT :: Show a => a -> Text
showT = T.pack . show
