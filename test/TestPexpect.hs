
{-# LANGUAGE OverloadedStrings #-}

module TestPexpect (initPexpectTest, pexpectTest) where

import Test.Hspec
    (SpecWith
    ,shouldReturn
    ,it
    ,describe
    )
import Pexpect
    (spawn
    ,exchange
    ,expect
    --,close
    ,initPexpect
    )

import Control.Monad (void)
import qualified Data.Text as T
    
initPexpectTest :: IO ()
initPexpectTest = initPexpect
 
pexpectTest :: SpecWith ()
pexpectTest = describe "pexpect" $ do

    let gp = "ghci> "
    
    describe "ghci init text" $ do
        it "starting text" $ (do
            h <- spawn "ghci"
            expect h gp)
            `shouldReturn` "GHCi, version 9.8.1: https://www.haskell.org/ghc/  :? for help\n"

    describe "ghci exchange" $ do
        it "exchange" $ (do
            h <- spawn "ghci"
            void $ expect h gp
            T.strip <$> exchange h gp "1 + 2")
            `shouldReturn` "3"

    describe "ghci error" $ do
        it "ghci error" $ (do
            h <- spawn "ghci"
            void $ expect h gp
            T.strip <$> exchange h gp "a")
            `shouldReturn` "<interactive>:1:1: error: [GHC-88464] Variable not in scope: a"

    describe "python exchange" $ do
        it "exchange" $ (do
            let pp = ">>> "
            h <- spawn "python3"
            void $ expect h pp
            T.strip <$> exchange h pp "1 + 2")
            `shouldReturn` "3"

    describe "exchange 3" $ do
        it "exchange" $ do
            let pp = ">>> "
            h <- spawn "python3"
            void $ expect h pp
            T.strip <$> exchange h pp "1 + 2"
              `shouldReturn` "3"

            T.strip <$> exchange h pp "4 + 5"
              `shouldReturn` "9"

            T.strip <$> exchange h pp "6 + 7"
              `shouldReturn` "13"



-- todo: figure out how to use shared state with hspec
-- figure out how to deal with exceptions better in this stuff
-- find or write an expectation that does a regex match and has nice failure messages
--   and a specialized isInfix of expectation that has a nice failure message
--   -> there's a shouldContain for lists, so can modify that for Text and regexes and T.isInfixOf


