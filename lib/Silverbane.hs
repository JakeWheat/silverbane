{-

main "api" (not really sure if this should be a lib)

-}
{-# LANGUAGE OverloadedStrings #-}
module Silverbane
    (checkDocument
    ,SilverbaneError(..)
    ,prettyError
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

import CheckAssertions
    (checkAssertions
    ,SilverbaneError(..)
    ,prettyError
    )
import qualified Parse as Pa
import qualified RunActions as R
import qualified DiffLibWrap as D

checkDocument :: String -> Text -> IO [SilverbaneError]
checkDocument fn input = do
    let fcs = either (errorT . Pa.prettyError) id $ Pa.parseFile fn input
    afcs <- R.runActions fn fcs
    D.initDiffLibWrap
    pure $ checkAssertions fn afcs

errorT :: HasCallStack => Text -> a
errorT = error . T.unpack
