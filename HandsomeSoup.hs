module HandsomeSoup where

import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Data.Tree.NTree.TypeDefs
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Maybe
import Text.Parsec
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Functor.Identity as I
import qualified Debug.Trace as D
import Data.List
import Control.Monad

-- if no tag name was given, sName will be set to '*'
-- attrs are (attr name, attr value).
-- if attr value is the empty string, we just check to
-- make sure that the element has that attribute.
-- if the attr value is prefixed with a '~', we treat
-- that attribute as a list of words separated by a space
-- and make sure that at least one of those words matches.
data Selector = Selector { sName :: String, sAttrs :: [(String,String)], sPseudoClasses :: [String] } | Space | ChildOf | FollowedBy

instance Show Selector where
  show (Selector name attrs pseudo) = show name ++ ":" ++ showMap attrs ++ ", " ++ show pseudo
      where showMap m = ("{" ++ (foldl (\acc (k,v) -> acc ++ (show k) ++ ":" ++ (show v) ++ ", ") "" m)) ++ "}"

combinator :: ParsecT [Char] u I.Identity Char
combinator = char ' ' <|> char '+' <|> char '>' <|> char ','

-- | selects a tag name, like "h1"
tag :: ParsecT [Char] u I.Identity [Char]
tag = many1 alphaNum

-- TODO match this syntax:
-- E:lang(c)	Matches element of type E if it is in (human) language c (the document language specifies how language is determined).
-- | selects a pseudo class, like ":link", ":first-child" etc.
pseudoClass :: ParsecT [Char] u I.Identity [Char]
pseudoClass = char ':' >> many1 (alphaNum <|> oneOf "-")

-- | class selector, selects ".foo"
klass :: ParsecT [Char] u I.Identity ([Char], [Char])
klass = do
    char '.' 
    val <- many1 alphaNum
    return ("class", '~':val)

-- | id selector, selects "#foo"
id_ :: ParsecT [Char] u I.Identity ([Char], [Char])
id_ = do
    char '#'
    val <- many1 alphaNum
    return ("id", val)

-- | selects attributes, like "[id]" (element must have id) or "[id=foo]" (element must have id foo).
tagAttribute :: ParsecT [Char] u I.Identity ([Char], [Char])
tagAttribute = 
    let f contents
          -- prefix the value with "~" before returning
          | "~=" `isInfixOf` contents = return $ (\(a, b) -> (a, '~':b)) $ splitOn "~=" contents
          | '=' `elem` contents = return $ splitOn "=" contents
          | otherwise = return (contents, "")
    in do
      contents <- between (char '[') (char ']') (many1 (alphaNum <|> oneOf "~="))
      f contents

-- like break, except don't keep the element you broke on.
-- and it takes a list as the thing to break on.
splitOn a xs = _splitOn a "" xs

_splitOn _ begin [] = (begin, [])
_splitOn a begin end@(x:xs)
  | a `isPrefixOf` end = (begin, drop (length a) end)
  | otherwise = _splitOn a (begin ++ [x]) xs

-- | universal selector, selects "*"
universalSelector :: ParsecT [Char] u I.Identity String
universalSelector = string "*"

-- | selects a tagname followed by one or more secondary selectors
-- | example: "a.foo", "*#hello", "h1" etc
tagSelector :: ParsecT [Char] u I.Identity Selector
tagSelector = do
    tagName <- tag <|> universalSelector
    attrs <- many1 (klass <|> id_ <|> tagAttribute) <|> (return [])
    pseudo <- many1 pseudoClass <|> (return [])
    return $ Selector tagName attrs pseudo

secondarySelector = do
    attrs <- many1 (klass <|> id_ <|> tagAttribute)
    pseudo <- many1 pseudoClass <|> (return [])
    return $ Selector "*" attrs pseudo

space_ = do
    string " "
    return Space

childOf = do
    string ">"
    return ChildOf

followedBy = do
    string "+"
    return FollowedBy

-- | A simple selector is either a type selector or universal selector followed immediately by zero or more attribute selectors, ID selectors, or pseudo-classes, in any order.
simpleSelector :: ParsecT [Char] u I.Identity Selector
simpleSelector = tagSelector <|> secondarySelector <|> space_ <|> childOf <|> followedBy

-- | One or more simple selectors separated by combinators
selector :: ParsecT [Char] u I.Identity [[Selector]]
selector = many1 simpleSelector `sepBy` (spaces >> string "," >> spaces)

css tag = case (parse selector "" tag) of
       Left err -> D.trace (show err) this
       Right x  -> fromSelectors (mconcat x)


-- make an arrow from selectors

-- TODO remember to account for "*"! The universal selector.
fromSelectors (s:selectors) = foldl (\acc selector -> make acc selector) (make none s) selectors
  where make acc sel@(Selector name attrs pseudo)
          | name == "*" = acc <+> (multi this >>> makeAttrs attrs)
          | otherwise = acc <+> ((D.trace $ show sel) $ multi $ hasName name >>> makeAttrs attrs)
        make acc Space = acc //> (D.trace "space" getChildren)
        make acc ChildOf = acc >>> (D.trace "childof" getChildren)
        makeAttrs (a:attrs) = foldl (\acc attr -> acc >>> makeAttr attr) (makeAttr a) attrs
        makeAttrs [] = this
        makeAttr (name, "") = hasAttr name
        makeAttr (name, '~':value) = hasAttrValue name (elem value . words)
        makeAttr (name, value) = hasAttrValue name (==value)


-- helper function for getting page content
openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
    Nothing -> fail "couldn't parse url"
    Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

-- given a url, returns a document
fromUrl :: String -> IO (IOSArrow XmlTree (NTree XNode))
fromUrl url = do
  contents <- runMaybeT $ openUrl url
  return $ parseHtml (fromMaybe "" contents)

parseHtml :: String -> IOSArrow XmlTree (NTree XNode)
parseHtml = readString [withParseHTML yes, withWarnings no]

(!) :: ArrowXml cat => cat a XmlTree -> String -> cat a String
(!) a str = a >>> getAttrValue str

-- str = "h1.class, h2#someid, h3[lang], h4:first-child > h5, h6 h7"
str = "h1.class, h2#someid"

main = do
  content <- readFile "test.html"
  let doc = parseHtml content
  links <- runX $ doc >>> css "p a" >>> getName
  print links


