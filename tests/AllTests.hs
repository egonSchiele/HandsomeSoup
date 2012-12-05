module AllTests where
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Control.Arrow
import qualified Data.Tree.NTree.TypeDefs as T
import System.IO.Unsafe

import Test.Hspec.Monadic

-- mk :: (Show a, Eq a, ArrowXml b) => String -> b (T.NTree XNode) (T.NTree XNode) -> a -> Test
run action = unsafePerformIO $ do
      doc <- fromFile "test.html"
      runX $ doc >>> action

main = hspec $ do
  describe "tests" $ do
    it "should get all links" $
      run (css "a" >>> getName) == ["a", "a", "a"]

    it "should get every element" $
      run (css "*" >>> getName) == ["/","html","head","title","body","h1","b","p","strong","a","strong","a","a","p","div","p"]

    it "should get descendents" $
      run (css "p strong" >>> getName) == ["strong", "strong"]

    it "should get children (negative test)" $
      run (css "p > strong" >>> getName) == ["strong"]

    it "should get children" $
      run (css "a > strong" >>> getName) == ["strong"]

    it ":first-child pseudo-element" $
      run (css "a:first-child" >>> getName) == ["a"]

    describe "attribute selector" $ do
      it "no value" $
        run (css "p[class]" >>> getName) == ["p"]

      it "exact value" $
        run (css "a[class=sister]" >>> getName) == ["a", "a"]

      it "inexact value" $
        run (css "a[class~=sister]" >>> getName) == ["a", "a", "a"]

      it "using |" $
        run (css "[lang|='en']" >>> getName) == ["html"]

      it "underscore in attribute selector should work" $
        run (css "div[class=curr_lang]" >>> getName) == ["div"]

      it "dash in attribute selector should work" $
        run (css "p[data-original=test]" >>> getName) == ["p"]

    it "class selector" $
      run (css "a.sister" >>> getName) == ["a", "a", "a"]

    it "id selector" $
      run (css "a#link1" >>> getName) == ["a"]

    it "both class and id selectors" $
      run (css "a.sister#link1" >>> getName) == ["a"]

    it "should ignore extra spaces" $
      run (css "html  body" >>> getName) == ["body"]

    it "should handle multiple elements" $
      run (css "a, p" >>> getName) == ["a","a","a","p","p","p"]

    it "should get grandchildren only" $
      run (css "p * strong" >>> getName) == ["strong"]
