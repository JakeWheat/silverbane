
module Main (main) where

import PexpectTest (initPexpectTest, pexpectTest)

import ParseTest (parseTests)

import Test.Hspec (hspec)

main :: IO ()
main = do
    initPexpectTest
    hspec $ do
        pexpectTest
        parseTests
    
