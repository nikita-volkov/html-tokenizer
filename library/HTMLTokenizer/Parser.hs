module HTMLTokenizer.Parser where

import BasePrelude
import Conversion
import Conversion.Text
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import HTMLEntities.Parser
import Data.Attoparsec.Text
import qualified Data.Text


data Token =
  Token_OpeningTag OpeningTag |
  Token_ClosingTag Identifier |
  Token_Text Text |
  Token_Comment Text 

type OpeningTag =
  (Identifier, [Attribute], Bool)

type Attribute =
  (Identifier, Maybe Text)

type Identifier =
  Text


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

closingTag :: Parser Identifier
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

