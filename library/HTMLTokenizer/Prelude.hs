module HTMLTokenizer.Prelude
( 
  module Exports,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- case-insensitive
-------------------------
import Data.CaseInsensitive as Exports (CI)

-- conversion
-------------------------
import Conversion as Exports
import Conversion.Text as Exports
import Conversion.CaseInsensitive as Exports

-- vector
-------------------------
import Data.Vector as Exports (Vector)
