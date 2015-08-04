module HTMLTokenizer
(
  -- * Model
  Token(..),
  OpeningTag,
  Identifier(..),
  Attribute,
  -- * Parsers
  token,
)
where

import BasePrelude hiding (takeWhile)
import Conversion
import Conversion.Text
import Conversion.CaseInsensitive
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.CaseInsensitive (CI)
import Data.Attoparsec.Text
import qualified Data.Text


-- |
-- An HTML token.
data Token =
  -- |
  -- A Doctype declaration.
  Token_Doctype Text |
  -- |
  -- An opening tag.
  Token_OpeningTag OpeningTag |
  -- |
  -- A closing tag.
  Token_ClosingTag Identifier |
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
  (Identifier, [Attribute], Bool)

-- |
-- A case-insensitive identifier.
data Identifier =
  Identifier (Maybe (CI Text)) (CI Text)
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

instance IsString Identifier where
  fromString =
    either (error "Invalid identifier") id .
    parseOnly identifier .
    convert

-- |
-- A tag attribute identifier and a value.
type Attribute =
  (Identifier, Maybe Text)

-- |
-- A token parser.
-- 
-- Does not decode entities.
token :: Parser Token
token =
  Token_Doctype <$> doctype <|>
  Token_Comment <$> comment <|>
  Token_ClosingTag <$> closingTag <|>
  Token_OpeningTag <$> openingTag <|>
  Token_Text <$> text

-- |
-- 
-- >>> parseOnly doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">"
-- Right "html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\""
doctype :: Parser Text
doctype =
  do
    string "<!"
    skipSpace
    asciiCI "doctype"
    space
    skipSpace
    contents <- takeWhile1 (/= '>')
    char '>'
    return contents

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

identifier :: Parser Identifier
identifier =
  Identifier <$> optional (component <* char ':') <*> component
  where
    component =
      fmap convert $ takeWhile1 $ flip any [isAlphaNum, flip elem ['_', '-']] . (&)

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

closingTag :: Parser Identifier
closingTag =
  string "</" *> skipSpace *> identifier <* skipSpace <* char '>'

text :: Parser Text
text =
  fmap ((convert :: Builder -> Text) . mconcat) $ many1 $
  convert <$> nonTagChar
  where
    nonTagChar =
      shouldFail comment *> shouldFail closingTag *> shouldFail openingTag *> shouldFail doctype *> anyChar

shouldFail :: Parser a -> Parser ()
shouldFail p =
  join $ (p $> empty) <|> pure (pure ())

