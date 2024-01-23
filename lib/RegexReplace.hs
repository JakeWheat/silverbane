
-- one liner in a normal language
-- either I missed something simple, or it ain't the year of haskell yet.

module RegexReplace
    (Re.Regex
    ,compile
    ,substitute
    ) where
    
import qualified Text.Regex.PCRE.ByteString.Utils as Re (substitute', compile', Regex)

import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Text (Text)

{-
main :: IO ()
main = do
    re <- compileRe "[0-9]+"
    res <- substitute re "21sdf321 12312 asdasd 123123" "XXY"
    T.putStrLn res-}

-- note: flips the args to be more haskell like
substitute :: Re.Regex -> Text -> Text -> Text
substitute re sub src =
    decodeUtf8Lenient $ either (error . show) id $ Re.substitute' re (encodeUtf8 src) (encodeUtf8 sub)

compile :: Text -> Re.Regex
compile b = either (error . show) id $ Re.compile' 0 0 (encodeUtf8 b)
