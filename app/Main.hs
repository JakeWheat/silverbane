{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Prelude as Pr
import Prelude hiding (error, show, putStrLn)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)

import qualified Pexpect as P

exchange :: P.Pexpect -> Text -> Text -> IO Text
exchange p prompt sl = do
    P.sendline p sl
    rep <- P.expect p prompt
    pure $ if sl `T.isPrefixOf` rep
        then T.drop (T.length sl) rep
        else rep

main :: IO ()
main = do
    P.initPexpect
    
    h <- P.spawn "ghci"
    putStrLn "spawn ghci"
    let prompt = "ghci> "
    it <- P.expect h prompt
    putStrLn it
    let ls = ["1 + 2", "putStrLn \"hello\""]
    flip mapM_ ls $ \l -> do
        --P.sendline h l
        rep <- exchange h prompt l
        putStrLn $ "Send: " <> l
        --ret <- P.expect h prompt
        putStrLn $ "reply: " <> rep
    (a,b) <- P.close h
    putStrLn $ "exit: " <> show (a,b)

-- todo: add callstack
error :: Text -> a
error = Pr.error . T.unpack

show :: Show a => a -> Text
show = T.pack . Pr.show
