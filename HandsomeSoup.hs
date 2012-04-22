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

-- if no tag name was given, sName will be set to '*'
-- attrs are (attr name, attr value).
-- if attr value is the empty string, we just check to
-- make sure that the element has that attribute.
data Selector = Selector { sName :: String, sAttrs :: [(String,String)], sPseudoClasses :: [String] }

instance Show Selector where
  show (Selector name attrs pseudo) = show name ++ ":" ++ showMap attrs ++ ", " ++ show pseudo
      where showMap m = (init.init $ "{" ++ (foldl (\acc (k,v) -> acc ++ (show k) ++ ":" ++ (show v) ++ ", ") "" m)) ++ "}"

combinator = char ' ' <|> char '+' <|> char '>' <|> char ','

-- | selects a tag name, like "h1"
tag :: ParsecT [Char] u I.Identity [Char]
tag = many1 alphaNum

-- | selects a pseudo class, like ":link", ":first-child" etc.
pseudoClass :: ParsecT [Char] u I.Identity [Char]
pseudoClass = char ':' >> many1 (alphaNum <|> oneOf "-")


-- | class selector, selects ".foo"
klass :: ParsecT [Char] u I.Identity ([Char], [Char])
klass = do
    char '.' 
    val <- many1 alphaNum
    return ("class", val)

-- | id selector, selects "#foo"
id_ :: ParsecT [Char] u I.Identity ([Char], [Char])
id_ = do
    char '#'
    val <- many1 alphaNum
    return ("id", val)

tagAttribute = do
    contents <- between (char '[') (char ']') (many1 (alphaNum <|> oneOf "="))
    if '=' `elem` contents
      then return $ splitOn '=' contents
      else return (contents, "")

-- like break, except don't keep the element you broke on.
splitOn a xs = (first, tail second)
    where (first, second) = break (==a) xs

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

-- | A simple selector is either a type selector or universal selector followed immediately by zero or more attribute selectors, ID selectors, or pseudo-classes, in any order.
simpleSelector :: ParsecT [Char] u I.Identity Selector
simpleSelector = tagSelector <|> secondarySelector

-- | One or more simple selectors separated by combinators
selector :: ParsecT [Char] u I.Identity [[Selector]]
selector = many1 simpleSelector `sepBy` (spaces >> combinator >> spaces)

css tag = case (parse selector "" tag) of
       Left err -> D.trace (show err) this
       Right x  -> fromSelectors (mconcat x)


-- make an arrow from selectors

-- TODO remember to account for "*"! The universal selector.
fromSelectors (s:selectors) = foldl (\acc selector -> acc <+> make selector) (make s) selectors
  where make sel@(Selector name attrs pseudo) 
          | name == "*" = multi this >>> makeAttrs attrs
          | otherwise = (D.trace $ show sel) $ multi $ hasName name >>> makeAttrs attrs
        makeAttrs (a:attrs) = foldl (\acc attr -> acc >>> makeAttr attr) (makeAttr a) attrs
        makeAttrs [] = this
        makeAttr (name, "") = hasAttr name
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
  links <- runX $ doc >>> css "a.sister[id=foo]:first-child:after" >>> getName
  print links


