module HTMLTokenizer.Parsing
(
  token,
)
where

import HTMLTokenizer.Prelude hiding (takeWhile)
import HTMLTokenizer.Data
import Data.Attoparsec.Text
import HTMLEntities.Parser (htmlEntity)
import qualified Text.Builder as A
import qualified HTMLTokenizer.MonadPlus as B
import qualified VectorBuilder.MonadPlus as C
import qualified Data.Text as D


{-|
Token parser, which also decodes entities.
-}
token :: Parser Token
token =
  openingTag OpeningTagToken <|>
  ClosingTagToken <$> closingTag <|>
  TextToken <$> textBetweenTags <|>
  CommentToken <$> comment <|>
  DoctypeToken <$> doctype

{-|
>>> parseOnly doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">"
Right "html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\""
-}
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

openingTag :: (Name -> Vector Attribute -> Bool -> openingTag) -> Parser openingTag
openingTag openingTag =
  do
    char '<'
    skipSpace
    theName <- name
    attributes <- C.many (space *> skipSpace *> attribute)
    skipSpace
    closed <- (char '/' $> True) <|> pure False
    char '>'
    return (openingTag theName attributes closed)

closingTag :: Parser Name
closingTag =
  string "</" *> skipSpace *> name <* skipSpace <* char '>'

textBetweenTags :: Parser Text
textBetweenTags =
  do
    prefixSpace <- (space *> skipSpace $> A.char ' ') <|> pure mempty
    text <- loop prefixSpace mempty
    if A.null text
      then mzero
      else return (A.run text)
  where
    loop !builder !unconsumedSpace =
      mplus word end
      where
        word =
          do
            parsedWord <- word
            space <- takeWhile isSpace
            if D.null space
              then return (builder <> A.text unconsumedSpace <> parsedWord)
              else loop (builder <> A.text unconsumedSpace <> parsedWord) space
          where
            word =
              B.concat1 (normalChunk <|> entity)
              where
                normalChunk =
                  A.text <$> takeWhile1 (\ x -> not (isSpace x) && x /= '<')
                entity =
                  A.text <$> htmlEntity
        end =
          if D.null unconsumedSpace
            then return builder
            else return (builder <> A.char ' ')

comment :: Parser Text
comment =
  string "<!--" *> (A.run <$> loop mempty)
  where
    loop !builder =
      do
        textWithoutDashes <- A.text <$> takeWhile (/= '-')
        mplus
          (string "-->" $> builder <> textWithoutDashes)
          (mplus
            (char '-' *> loop (builder <> textWithoutDashes <> A.char '-'))
            (return (builder <> textWithoutDashes)))
      where
        textWithoutDashes =
          A.text <$> takeWhile1 (/= '-')

attribute :: Parser Attribute
attribute =
  do
    attributeName <- name
    mplus
      (do
        skipSpace
        char '='
        skipSpace
        attributeValue <- msum (map quotedContent ['"', '\'', '`']) <|> unquotedContent
        return (Attribute attributeName attributeValue))
      (return (Attribute attributeName ""))

quotedContent :: Char -> Parser Text
quotedContent quotChar =
  char quotChar *> (A.run <$> B.concat escapedContentChunk) <* char quotChar
  where
    escapedContentChunk =
      normal <|> entity <|> escaped <|> failedEscaping
      where
        normal =
          A.text <$> takeWhile1 (\ x -> x /= quotChar && x /= '&' && x /= '\\')
        entity =
          A.text <$> htmlEntity
        escaped =
          char '\\' *> (A.char <$> satisfy (\ x -> x == quotChar || x == '\\'))
        failedEscaping =
          A.char <$> char '\\'

unquotedContent :: Parser Text
unquotedContent =
  isolatedTextInsideTag

name :: Parser Name
name =
  do
    c1 <- isolatedTextInsideTag
    skipSpace
    (mplus
      (do
        char ':'
        skipSpace
        c2 <- isolatedTextInsideTag
        return (PrefixedName c1 c2))
      (return (UnprefixedName c1)))

isolatedTextInsideTag :: Parser Text
isolatedTextInsideTag =
  A.run <$> B.concat1 (normal <|> entity)
  where
    normal =
      A.text <$> takeWhile1 predicate
      where
        predicate x =
          x /= '>' && x /= '/' && not (isSpace x) &&
          x /= '&' && x /= '<' &&
          x /= '"' && x /= '\'' && x /= '`'
    entity =
      A.text <$> htmlEntity

shouldFail :: Parser a -> Parser ()
shouldFail p =
  join ((p $> empty) <|> pure (pure ()))

skipSpaceLeaving1 :: Parser ()
skipSpaceLeaving1 =
  mplus
    (do
      space
      peekedChar <- peekChar'
      if isSpace peekedChar
        then skipSpaceLeaving1
        else mzero)
    (return ())
    