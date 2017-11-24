module HTMLTokenizer.Rendering.Text
where

import HTMLTokenizer.Prelude
import HTMLTokenizer.Data.Types
import qualified Text.Builder as A
import qualified HTMLTokenizer.Rendering.Text.Builder as B


token :: Token -> Text
token = A.run . B.token

attribute :: Attribute -> Text
attribute = A.run . B.attribute

name :: Name -> Text
name = A.run . B.name
