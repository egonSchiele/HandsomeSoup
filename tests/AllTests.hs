module AllTests where
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Control.Arrow
import Test.HUnit
import qualified Data.Tree.NTree.TypeDefs as T

t msg a b = TestCase $ assertEqual msg a b

file = do
    contents <- readFile "test.html"
    return $ parseHtml contents

-- mk :: (Show a, Eq a, ArrowXml b) => String -> b (T.NTree XNode) (T.NTree XNode) -> a -> Test
mk msg action expected = TestCase $ do
      doc <- file
      actual <- runX $ doc >>> action
      assertEqual msg expected actual

testSingleTypeSelector = mk "should get all links" (css "a" >>> getName) ["a", "a", "a"]

testUniversalSelector = mk "should get every element" (css "*" >>> getName) ["/","html","head","title","body","h1","b","p","strong","a","strong","a","a","p","div","p"]

testDescendents = mk "should get descendents" (css "p strong" >>> getName) ["strong", "strong"]
testChildren = mk "should get children (negative test)" (css "p > strong" >>> getName) ["strong"]
testChildren2 = mk "should get children" (css "a > strong" >>> getName) ["strong"]

testFirstChild = mk ":first-child pseudo-element" (css "a:first-child" >>> getName) ["a"]

testAttrSelector1 = mk "p[class]" (css "p[class]" >>> getName) ["p"]
testAttrSelector2 = mk "a[class=sister]" (css "a[class=sister]" >>> getName) ["a", "a"]
testAttrSelector3 = mk "a[class~=sister]" (css "a[class~=sister]" >>> getName) ["a", "a", "a"]
testAttrSelector4 = mk "[lang|='en']" (css "[lang|='en']" >>> getName) ["html"]
testAttrSelector5 = mk "div[class=curr_lang]" (css "div[class=curr_lang]" >>> getName) ["div"]
testAttrSelector6 = mk "div[data-original=test]" (css "div[data-original=test]" >>> getName) ["p"]

testClassSelector = mk "a.sister" (css "a.sister" >>> getName) ["a", "a", "a"]
testIdSelector = mk "a#link1" (css "a#link1" >>> getName) ["a"]

testIdAndClass = mk "should work even when both are specified" (css "a.sister#link1" >>> getName) ["a"]

testExtraSpaces = mk "should ignore extra spaces" (css "html  body" >>> getName) ["body"]

testMultipleElements = mk "should handle multiple elements" (css "a, p" >>> getName) ["a","a","a","p","p","p"]

testGrandChildren = mk "should get grandchildren only" (css "p * strong" >>> getName) ["strong"]


main = runTestTT $ TestList [testSingleTypeSelector, testUniversalSelector, testDescendents, testChildren, testChildren2, testFirstChild, testAttrSelector1, testAttrSelector2, testAttrSelector3, testAttrSelector4, testAttrSelector5, testAttrSelector6, testClassSelector, testIdSelector, testIdAndClass, testExtraSpaces, testMultipleElements, testGrandChildren]
