
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

{-
TODO:
show unmatched lines from both sides

want to still show the matched ones too because this can help with debugging a test
-}
expectErrorsShouldMatch ::
    HasCallStack =>
    IO [E.ExpectTestError] ->
    [Text] ->
    Expectation
expectErrorsShouldMatch gotA [] = do
    got <- gotA
    let ps = map E.prettyExpectError got
    unless (null got) $
        expectationFailure
        $ "unexpected errors:\n"
           <> unlines (map T.unpack ps)
expectErrorsShouldMatch gotA expectedMatches = do
    got <- gotA
    let ps = map E.prettyExpectError got
        foundMatch em = or $ map (em `T.isInfixOf`) ps
        passes = and $ map foundMatch expectedMatches
    unless passes $
        expectationFailure
        $ "expected:\n" <> show expectedMatches <> "\ngot:\n"
           <> unlines (map T.unpack ps)
