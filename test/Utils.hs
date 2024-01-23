
{-# LANGUAGE OverloadedStrings #-}
module Utils
    (shouldFailContains
    ,expectErrorsShouldMatch
    ) where

import Test.Hspec
    (HasCallStack
    ,Expectation
    ,expectationFailure
    )

import Text.Megaparsec
    (ShowErrorComponent
    ,VisualStream
    ,TraversableStream
    ,ParseErrorBundle
    ,errorBundlePretty
    )
import qualified Data.Text as T
import Data.Text (Text)

import Control.Monad (unless)
import ExpectTest as E

shouldFailContains ::
    (HasCallStack, ShowErrorComponent e,
    VisualStream s,TraversableStream s, Show a) =>
    Either (ParseErrorBundle s e) a ->
    Text ->
    Expectation
shouldFailContains r errText = case r of
  Left e0 ->
    let se = errorBundlePretty e0
     in unless (errText `T.isInfixOf` T.pack se) . expectationFailure $
          "the parser is expected to fail with error containing:\n"
            ++ T.unpack errText
            ++ "\nbut it failed with:\n"
            ++ se
  Right v -> expectationFailure $
      "the parser is expected to fail, but it parsed: " ++ show v

expectErrorsShouldMatch ::
    IO [E.ExpectTestError] ->
    [Text] ->
    Expectation
expectErrorsShouldMatch gotA expectedMatches = do
    got <- gotA
    let ps = map E.prettyExpectError got
        passes =
            if null expectedMatches
            then null got
            else let foundMatch em = or $ map (em `T.isInfixOf`) ps
                 in and $ map foundMatch expectedMatches
    unless passes $
        expectationFailure $ show expectedMatches <> "\n"
           <> unlines (map T.unpack ps)
