{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Prelude as Pr
import Prelude hiding (error, show, putStrLn)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)

import qualified Pexpect as P

main :: IO ()
main = do
    P.initPexpect
    
    h <- P.spawn "ghci"
    putStrLn "spawn ghci"
    let prompt = "ghci> "
    it <- P.expect h prompt
    putStrLn it
    let ls = ["1 + 2", "putStrLn \"hello\\nstuff\""]
    flip mapM_ ls $ \l -> do
        --P.sendline h l
        rep <- P.exchange h prompt l
        putStrLn $ "Send: " <> l
        --ret <- P.expect h prompt
        putStrLn $ "reply: " <> T.strip rep

        --let str = T.unpack rep
        --    showIt n c = Pr.putStrLn $ Pr.show n ++ " ---- " ++ " " ++ Pr.show (ord c) ++ " ----\n\n"
        --zipWithM_ showIt [(0::Int)..] str
        --flip mapM_ str $ \c -> 

    (a,b) <- P.close h
    putStrLn $ "exit: " <> show (a,b)

-- todo: add callstack
error :: Text -> a
error = Pr.error . T.unpack

show :: Show a => a -> Text
show = T.pack . Pr.show
