module HTMLTokenizer.Rendering.Text.Builder
(
  token,
  attribute,
  name,
)
where

import HTMLTokenizer.Prelude hiding (takeWhile)
import HTMLTokenizer.Data.Types
import Text.Builder
import qualified Data.Text as D
import qualified HTMLEntities.Text as B


encodedText :: Text -> Builder
encodedText =
  text . B.text

name :: Name -> Builder
name =
  \ case
    UnprefixedName localName ->
      encodedText localName
    PrefixedName space localName ->
      encodedText space <> char ':' <> encodedText localName

attribute :: Attribute -> Builder
attribute (Attribute attributeName content) =
  if D.null content
    then name attributeName
    else name attributeName <> text "=\"" <> encodedText content <> char '"'

token :: Token -> Builder
token =
  \ case
    OpeningTagToken tagName attrVec closed ->
      char '<' <> name tagName <> attrs <> closing
      where
        attrs =
          foldMap (\ x -> char ' ' <> attribute x) attrVec
        closing =
          if closed
            then text "/>"
            else char '>'
    ClosingTagToken tagName ->
      text "</" <> name tagName <> char '>'
    TextToken theText ->
      encodedText theText
    CommentToken commentText ->
      text "<!--" <> encodedText commentText <> text "-->"
    DoctypeToken doctypeText ->
      text "<!doctype" <>
      (if D.null doctypeText then mempty else char ' ' <> encodedText doctypeText) <>
      char '>'
