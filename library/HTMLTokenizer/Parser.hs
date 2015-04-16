module HTMLTokenizer.Parser
(
  -- * Model
  Token(..),
  OpeningTag,
  Attribute,
  -- * Parsers
  token,
)
where

import BasePrelude
import Conversion
import Conversion.Text
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import HTMLEntities.Parser
import Data.Attoparsec.Text
import qualified Data.Text


-- |
-- An HTML token.
data Token =
  -- |
  -- An opening tag.
  Token_OpeningTag OpeningTag |
  -- |
  -- A closing tag name.
  Token_ClosingTag Text |
  -- |
  -- A text between tags with HTML-entities decoded.
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
-- A tag attribute identifier and a value with HTML-entities decoded.
type Attribute =
  (Text, Maybe Text)

-- |
-- A token parser.
token :: Parser Token
token =
  skipSpace *> (
    Token_Comment <$> comment <|>
    Token_ClosingTag <$> closingTag <|>
    Token_OpeningTag <$> openingTag <|>
    Token_Text <$> text
  )

openingTag :: Parser OpeningTag
openingTag =
  do
    char '<'
    skipSpace
    name <- identifier
    attributes <- many $ space *> skipSpace *> attribute
    skipSpace
    closed <- convert <$> optional (char '/')
    char '>'
    return (name, attributes, closed)

attribute :: Parser Attribute
attribute =
  do
    identifierValue <- identifier
    value <-
      optional $ do
        skipSpace
        char '='
        skipSpace
        quotedValue '"' <|> quotedValue '\'' <|> unquotedValue
    return (identifierValue, value)
  where
    quotedValue q =
      do
        char q
        value <- 
          fmap ((convert :: Builder -> Text) . mconcat) $ many $ 
          fmap convert htmlEntity <|> fmap convert (satisfy (/= q))
        char q
        return value
    unquotedValue =
      takeWhile1 isAlphaNum

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
  convert <$> htmlEntity <|> convert <$> nonTagChar
  where
    nonTagChar =
      shouldFail comment *> shouldFail closingTag *> shouldFail openingTag *> anyChar

shouldFail :: Parser a -> Parser ()
shouldFail p =
  ((p $> False) <|> pure True) >>= bool empty (pure ())

