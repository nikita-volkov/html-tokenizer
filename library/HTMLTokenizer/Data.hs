{-# OPTIONS_GHC -funbox-strict-fields #-}
module HTMLTokenizer.Data
where

import HTMLTokenizer.Prelude


data Token =
  OpeningTagToken !Name !(Vector Attribute) !Bool |
  ClosingTagToken !Name |
  SpaceToken |
  TextToken !Name |
  CommentToken !Text |
  DoctypeToken !Text

data Name =
  UnprefixedName !Text |
  PrefixedName !Text !Text

data Attribute =
  Attribute !Name !Text
