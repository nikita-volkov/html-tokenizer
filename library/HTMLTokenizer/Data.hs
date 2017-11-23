{-# OPTIONS_GHC -funbox-strict-fields #-}
module HTMLTokenizer.Data
where

import HTMLTokenizer.Prelude


data Token =
  OpeningTagToken !Name !(Vector Attribute) !Bool |
  ClosingTagToken !Name |
  TextToken !Text |
  CommentToken !Text |
  DoctypeToken !Text
  deriving (Show)

data Name =
  UnprefixedName !Text |
  PrefixedName !Text !Text
  deriving (Show)

data Attribute =
  Attribute !Name !Text
  deriving (Show)
