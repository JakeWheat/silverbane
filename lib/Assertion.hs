
{-

Assertion types: This represents the tests that are parsed from a document file.

-}

module Assertion
    (FileChunk(..)
    ,EtFile(..)
    ,EtRun(..)
    ,EtSession(..)
    ,EtContinue(..)
    ,SessionLine(..)
    ) where

import Data.Text (Text)

data FileChunk
    = FcFile EtFile
    | FcRun EtRun
    | FcSession EtSession
    | FcContinue EtContinue
      deriving (Eq,Show)

data EtFile
    = EtFile
    {efStartLine :: Int
    ,efFilename :: Text
    ,efBody :: Text
    }
      deriving (Eq,Show)

data EtRun
    = EtRun
    {erStartLine :: Int
    ,erCwd :: Maybe Text
    ,erCmd :: Text
    ,erZeroExit :: Bool
    ,erBody :: Text
    }
      deriving (Eq,Show)

data EtSession
    = EtSession
    {esStartLine :: Int
    ,esCwd :: Maybe Text
    ,esCmd :: Text
    ,esPrompt :: Text
    ,esInitialText :: Maybe Bool
    ,esFilters :: [(Text,Text)]
    ,esSessionLines :: [SessionLine]
    }
      deriving (Eq,Show)

data SessionLine
    = Prompt Text
    | Reply Text
      deriving (Eq,Show)

data EtContinue
    = EtContinue
    {ecStartLine :: Int
    ,ecSessionLines :: [SessionLine]
    }
      deriving (Eq,Show)
