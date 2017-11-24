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
  labeled "HTML Token" $
  join
    (mplus
      (do
        char '<'
        mplus
          (do
            char '!'
            mplus
              (string "--" $> commentTagBody)
              (asciiCI "doctype" $> doctypeTagBody))
          (mplus
            (char '/' $> closingTagBody)
            (pure openingTagBody)))
      (pure (TextToken <$> textBetweenTags)))
  where
    commentTagBody =
      labeled "Comment" (CommentToken . A.run <$> loop mempty)
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
    doctypeTagBody =
      labeled "Doctype" $ do
        space
        skipSpace
        contents <- takeWhile1 (/= '>')
        char '>'
        return (DoctypeToken contents)
    closingTagBody =
      labeled "Closing tag" $
      skipSpace *> (ClosingTagToken <$> name) <* skipSpace <* char '>'
    openingTagBody =
      labeled "Opening tag" $ do
        skipSpace
        tagName <- name
        attributes <- C.many (space *> skipSpace *> attribute)
        skipSpace
        closed <- (char '/' $> True) <|> pure False
        char '>'
        return (OpeningTagToken tagName attributes closed)

textBetweenTags :: Parser Text
textBetweenTags =
  labeled "Text between tags" $ do
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
              B.concat1 (normalChunk <|> entity <|> ampersand)
              where
                normalChunk =
                  A.text <$> takeWhile1 (\ x -> not (isSpace x) && x /= '<' && x /= '&')
                entity =
                  A.text <$> htmlEntity
                ampersand =
                  A.char <$> char '&'
        end =
          if D.null unconsumedSpace
            then return builder
            else return (builder <> A.char ' ')

attribute :: Parser Attribute
attribute =
  labeled "Attribute" $ do
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
  labeled "Name" $ do
    c1 <- D.toLower <$> isolatedTextInsideTag
    (mplus
      (do
        skipSpace
        char ':'
        skipSpace
        c2 <- D.toLower <$> isolatedTextInsideTag
        return (PrefixedName c1 c2))
      (return (UnprefixedName c1)))

isolatedTextInsideTag :: Parser Text
isolatedTextInsideTag =
  A.run <$> B.concat1 (normal <|> entity <|> ampersand)
  where
    normal =
      A.text <$> takeWhile1 predicate
      where
        predicate x =
          x /= '>' && x /= '/' && not (isSpace x) && x /= '=' &&
          x /= '&' && x /= '<' &&
          x /= '"' && x /= '\'' && x /= '`'
    entity =
      A.text <$> htmlEntity
    ampersand =
      A.char <$> char '&'

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

{-# INLINE labeled #-}
labeled :: String -> Parser a -> Parser a
labeled label parser =
  parser <?> label
