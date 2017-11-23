module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified HTMLTokenizer.Data as A
import qualified HTMLTokenizer.Parsing as B
import qualified Data.Attoparsec.Text as C
import qualified Data.Vector as D


main =
  defaultMain $
  testGroup "All tests" $
  [
    testTokenParsing "Opening tag"
      (Right (A.OpeningTagToken (A.UnprefixedName "script") (D.fromList []) False))
      "<script>"
    ,
    testTokenParsing "Opening tag with arguments"
      (Right
        (A.OpeningTagToken
          (A.UnprefixedName "script")
          (D.fromList
            [
              A.Attribute (A.UnprefixedName "type") "text/javascript",
              A.Attribute (A.UnprefixedName "src") "https://optimize-stats.voxmedia.com/loader.min.js?key=efd28c71b5699c36"
            ])
          False))
      "<script type=\"text/javascript\" src=\"https://optimize-stats.voxmedia.com/loader.min.js?key=efd28c71b5699c36\">"
    ,
    testTokenParsing "Closed opening tag"
      (Right (A.OpeningTagToken (A.UnprefixedName "script") (D.fromList []) True))
      "<script/>"
    ,
    testTokenParsing "Spaces"
      (Right (A.OpeningTagToken (A.UnprefixedName "script") (D.fromList []) True))
      "<  script  />"
    ,
    testTokenParsing "Entities in names"
      (Right (A.OpeningTagToken (A.UnprefixedName "xxr") (D.fromList []) True))
      "<x&#120;r/>"
    ,
    testTokenParsing "Entities in attributes"
      (Right (A.OpeningTagToken (A.UnprefixedName "x") (D.fromList [A.Attribute (A.UnprefixedName "attr") "YyY"]) True))
      "<x attr=\"Y&#121;Y\"/>"
    ,
    testTokenParsing "Empty attributes"
      (Right (A.OpeningTagToken (A.UnprefixedName "x") (D.fromList [A.Attribute (A.UnprefixedName "attr") ""]) True))
      "<x attr/>"
    ,
    testTokenParsing "Closing tag"
      (Right (A.ClosingTagToken (A.UnprefixedName "x")))
      "</x>"
    ,
    testTokensParsing "Text between tags, stripped"
      (Right
        [
          A.OpeningTagToken (A.UnprefixedName "x") mempty False,
          A.TextToken "abc",
          A.ClosingTagToken (A.UnprefixedName "x")
        ])
      "<x>abc</x>"
    ,
    testTokensParsing "Text between tags, unstripped"
      (Right
        [
          A.OpeningTagToken (A.UnprefixedName "x") mempty False,
          A.TextToken " abc ",
          A.ClosingTagToken (A.UnprefixedName "x")
        ])
      "<x>   abc \n </x>"
    ,
    testTokensParsing "Text between tags, unstripped"
      (Right
        [
          A.OpeningTagToken (A.UnprefixedName "x") mempty False,
          A.TextToken " abc ",
          A.ClosingTagToken (A.UnprefixedName "x")
        ])
      "<x>   abc \n </x>"
    ,
    testTokensParsing "Text between tags, entities"
      (Right
        [
          A.OpeningTagToken (A.UnprefixedName "x") mempty False,
          A.TextToken "a<c x",
          A.ClosingTagToken (A.UnprefixedName "x")
        ])
      "<x>a&lt;c &#120;</x>"
    ,
    testTokensParsing "Text between tags, broken entities"
      (Right
        [
          A.OpeningTagToken (A.UnprefixedName "x") mempty False,
          A.TextToken "a<c & #120;",
          A.ClosingTagToken (A.UnprefixedName "x")
        ])
      "<x>a&lt;c & #120;</x>"
  ]
  where
    testTokenParsing name expectedResult text =
      testCase name $
      assertEqual "" expectedResult (C.parseOnly (B.token <* C.endOfInput) text)
    testTokensParsing name expectedResult text =
      testCase name $
      assertEqual "" expectedResult (C.parseOnly (many B.token <* C.endOfInput) text)

