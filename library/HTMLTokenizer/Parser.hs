module HTMLTokenizer.Parser
(
  -- * Model
  Token(..),
  OpeningTag,
  Text,
  Attribute,
  -- * Parsers
  token,
)
where

import BasePrelude hiding (takeWhile)
import Conversion
import Conversion.Text
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Attoparsec.Text
import qualified Data.Text


-- |
-- An HTML token.
data Token =
  -- |
  -- An opening tag.
  Token_OpeningTag OpeningTag |
  -- |
  -- A closing tag.
  Token_ClosingTag Text |
  -- |
  -- A text between tags.
  Token_Text Text |
  -- |
  -- Contents of a comment.
  Token_Comment Text 
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

-- |
-- An opening tag name, attributes and whether it is closed.
type OpeningTag =
  (Text, [Attribute], Bool)

-- |
-- A tag attribute identifier and a value.
type Attribute =
  (Text, Maybe Text)

-- |
-- A token parser.
-- 
-- Does not decode entities.
token :: Parser Token
token =
  Token_Comment <$> comment <|>
  Token_ClosingTag <$> closingTag <|>
  Token_OpeningTag <$> openingTag <|>
  Token_Text <$> text

openingTag :: Parser OpeningTag
openingTag =
  do
    char '<'
    skipSpace
    theIdentifier <- identifier
    attributes <- many $ space *> skipSpace *> attribute
    skipSpace
    closed <- convert <$> optional (char '/')
    char '>'
    return (theIdentifier, attributes, closed)

attribute :: Parser Attribute
attribute =
  do
    theIdentifier <- identifier
    value <-
      optional $ do
        skipSpace
        char '='
        skipSpace
        msum (map quotedValue ['"', '\'', '`']) <|> entityQuotedValue <|> unquotedValue
    return (theIdentifier, value)
  where
    quotedValue q =
      char q *> takeWhile (/= q) <* char q
    unquotedValue =
      takeWhile1 $ flip all [not . isSpace, not . flip elem ['=', '<', '>', '/']] . (&)
    -- |
    -- For some really messed-up HTML.
    entityQuotedValue =
      fmap convert $ q *> manyTill' anyChar q
      where
        q = asciiCI "&quot;"

identifier :: Parser Text
identifier = 
  takeWhile1 (flip any [isAlphaNum, flip elem ['_', '-', '!', '?']] . flip ($))

comment :: Parser Text
comment =
  (convert :: Builder -> Text) <$> (string "<!--" *> content)
  where
    content =
      (liftA2 mappend
        (fmap convert (takeWhile1 (/= '-')))
        (mplus
          (fmap (const mempty) (string "-->"))
          (liftA2 mappend
            (fmap convert (char '-'))
            (content))))

closingTag :: Parser Text
closingTag =
  string "</" *> skipSpace *> identifier <* skipSpace <* char '>'

text :: Parser Text
text =
  fmap ((convert :: Builder -> Text) . mconcat) $ many1 $
  convert <$> nonTagChar
  where
    nonTagChar =
      shouldFail comment *> shouldFail closingTag *> shouldFail openingTag *> anyChar

shouldFail :: Parser a -> Parser ()
shouldFail p =
  join $ (p $> empty) <|> pure (pure ())

