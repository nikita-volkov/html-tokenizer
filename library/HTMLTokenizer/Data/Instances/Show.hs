module HTMLTokenizer.Data.Instances.Show
where

import HTMLTokenizer.Prelude
import HTMLTokenizer.Data.Types
import qualified HTMLTokenizer.Rendering.Text as A


instance Show Token where
  show = textString . A.token

instance Show Name where
  show = textString . A.name

instance Show Attribute where
  show = textString . A.attribute
