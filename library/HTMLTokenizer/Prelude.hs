module HTMLTokenizer.Prelude
( 
  module Exports,
  textString,
  showText,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding ((<>), First(..), Last(..))

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- vector
-------------------------
import Data.Vector as Exports (Vector)

--------------------------------------------------------------------------------

import qualified Data.Text as A

textString :: Text -> String
textString =
  A.unpack

showText :: Show a => a -> Text
showText =
  fromString . show
