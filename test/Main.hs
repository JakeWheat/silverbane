
module Main (main) where

import TestPexpect (initPexpectTest, pexpectTest)

import TestParse (parseTests)
import  TestExpectTest (expectTestTests)

import Test.Hspec (hspec)

main :: IO ()
main = do
    initPexpectTest
    hspec $ do
        pexpectTest
        parseTests
        expectTestTests
