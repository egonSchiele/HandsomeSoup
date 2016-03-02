module Text.HandsomeSoup (openUrl, fromUrl, parseHtml, (!), css) where

import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Data.Tree.NTree.TypeDefs
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Maybe
import Text.Parsec
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Functor.Identity as I
import qualified Debug.Trace as D
import Data.List
import Control.Monad
import Text.CSS.Parser hiding (css)
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.HTTP

-- | Helper function for getting page content. Example:
--
-- > contents <- runMaybeT $ openUrl "http://foo.com"
openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail "couldn't parse url"
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

-- | Given a url, returns a document. Example:
--
-- > doc = fromUrl "http://foo.com"
-- > doc = fromUrl "tests/test.html"
fromUrl :: String -> IOSArrow b (NTree XNode)
fromUrl url = readDocument [withValidate        no,
                            withInputEncoding   utf8,
                            withParseByMimeType yes,
                            withHTTP            [],
                            withWarnings        no] url

-- | Given a string, parses it and returns a document. Example:
--
-- > doc = parseHtml "<h1>hello!</h1>"
parseHtml :: String -> IOSArrow b (NTree XNode)
parseHtml = readString [withParseHTML yes, withWarnings no]

-- | Shortcut for getting attributes. Example:
--
-- > doc >>> css "a" ! "href"
(!) :: ArrowXml cat => cat a XmlTree -> String -> cat a String
(!) a str = a >>> getAttrValue str

-- | A css selector for getting elements from a document. Example:
--
-- > doc >>> css "#menu li"
css :: ArrowXml a => [Char] -> a (NTree XNode) (NTree XNode)
css tag = case (parse selector "" tag) of
       Left err -> D.trace (show err) this
       Right x  -> fromSelectors x

-- | Used internally. works on a selector (i.e a list of simple selectors)
fromSelectors sel@(s:selectors) = foldl (\acc selector -> acc <+> _fromSelectors selector) (_fromSelectors s) selectors

-- | Used internally. works on simple selectors and their combinators
_fromSelectors (s:selectors) = foldl (\acc selector -> make acc selector) (make this s) selectors
  where 
        make acc sel@(Selector name attrs pseudo)
          | name == "*" = acc >>> ((multi this >>> makeAttrs attrs) >>. makePseudos pseudo)
          | otherwise = acc >>> ((multi $ hasName name >>> makeAttrs attrs) >>. makePseudos pseudo)
        make acc Space = acc >>> getChildren
        make acc ChildOf = acc >>> getChildren >>> processChildren none
        makeAttrs (a:attrs) = foldl (\acc attr -> acc >>> makeAttr attr) (makeAttr a) attrs
        makeAttrs [] = this
        makeAttr (name, "") = hasAttr name
        makeAttr (name, '~':value) = hasAttrValue name (elem value . words)
        makeAttr (name, '|':value) = hasAttrValue name (headMatch value)
        makeAttr (name, value) = hasAttrValue name (==value)
        makePseudos (p:pseudos) = foldl (\acc pseudo -> acc >>> makePseudo pseudo) (makePseudo p) pseudos
        makePseudos [] = id
        makePseudo selector
            | selector == "first-child" = take 1
            | nthSelector selector = take 1 . drop (n - 1)
            where nthSelector selector = take 10 selector == "nth-child("
                  getN selector = read $ (takeWhile (/= ')') . drop 10) selector :: Int
                  n = getN selector

-- | Used internally to match attribute selectors like @ [att|=val] @.
-- From: http://www.w3.org/TR/CSS2/selector.html
-- "Represents an element with the att attribute, its value either being exactly "val" or beginning with "val" immediately followed by '-'".
headMatch value attrValue = value == attrValue || value `isPrefixOf` attrValue
    where first = head . words $ attrValue
