module HTMLTokenizer.Prelude
( 
  module Exports,
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
