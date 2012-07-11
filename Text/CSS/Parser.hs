module Text.CSS.Parser where

import Text.Parsec
import qualified Data.Functor.Identity as I
import Data.List
import Data.Maybe
import Text.CSS.Utils
-- if no tag name was given, sName will be set to '*'
-- attrs are (attr name, attr value).
-- if attr value is the empty string, we just check to
-- make sure that the element has that attribute.
--
-- if the attr value is prefixed with a '~', we treat
-- that attribute as a list of words separated by a space
-- and make sure that at least one of those words matches.
-- 
-- if the attr value is prefixed with a `|`, the value must
-- be exactly val or start with val immediately followed by a '-'.
-- This is primarily intended to allow language subcode matches.
-- Example: [lang|="en"] matches "en", "en-US", etc.
-- From: http://www.w3.org/TR/CSS2/selector.html.
data Selector = Selector { sName :: String, sAttrs :: [(String,String)], spseudoSelectores :: [String] } | Space | ChildOf | FollowedBy deriving (Show)

-- pretty printers for debugging
pp Space = "<space>"
pp ChildOf = "<child of>"
pp FollowedBy = "<followed by>"
pp (Selector name attrs pseudo) = show name ++ ":" ++ showMap attrs ++ ", " ++ show pseudo
    where showMap m = ("{" ++ (foldl (\acc (k,v) -> acc ++ (show k) ++ ":" ++ (show v) ++ ", ") "" m)) ++ "}"

{- Some lexeme parsers -}
ident :: ParsecT [Char] u I.Identity String
ident = do c1 <- optionMaybe (char '-')
           c2 <- nmstart
           cs <- many nmchar
           return $ concat [maybeToList c1, [c2], cs]

nmstart = alphaNum <|> char '_'
nmchar  = alphaNum <|> oneOf "_-"

{- TYPE SELECTORS FOLLOW -}

-- | selects a tag name, like @ h1 @
typeSelector :: ParsecT [Char] u I.Identity [Char]
typeSelector = many1 (alphaNum <|> oneOf "_-")

-- | universal selector, selects @ * @
universalSelector :: ParsecT [Char] u I.Identity String
universalSelector = string "*"

{- SECONDARY SELECTORS FOLLOW -}

-- | selects a pseudo-element or pseudo-class, like @ :link @, @ :first-child @ etc.
pseudoSelector :: ParsecT [Char] u I.Identity [Char]
pseudoSelector = char ':' >> many1 (alphaNum <|> oneOf "-()")

-- | class selector, selects @ .foo @
classSelector :: ParsecT [Char] u I.Identity ([Char], [Char])
classSelector = do
    val <- char '.' >> ident
    return ("class", '~':val)

-- | id selector, selects @ #foo @
idSelector :: ParsecT [Char] u I.Identity ([Char], [Char])
idSelector = do
    val <- char '#' >> many1 nmchar
    return ("id", val)

-- | selects attributes, like @ [id] @ (element must have id) or @ [id=foo] @ (element must have id foo).
attributeSelector :: ParsecT [Char] u I.Identity ([Char], [Char])
attributeSelector = do
      _contents <- between (char '[') (char ']') (many1 (alphaNum <|> oneOf "/-_|~=\"'."))
      -- remove quotes
      let contents = filter (\c -> c /= '"' && c /= '\'') _contents
      if "~=" `isInfixOf` contents 
          then return $ (\(a, b) -> (a, '~':b)) $ splitOn "~=" contents
          else if "|=" `isInfixOf` contents 
              then return $ (\(a, b) -> (a, '|':b)) $ splitOn "|=" contents
              else if '=' `elem` contents
                   then return $ splitOn "=" contents
                   else return (contents, "")

-- | selector for everything after the type except pseudoSelectores
secondarySelector = many1 (classSelector <|> idSelector <|> attributeSelector)

{- COMBINATOR SELECTORS FOLLOW -}

space_ = do
    many1 $ string " "
    return Space

childOf = do
    spaces >> string ">" >> spaces
    return ChildOf

followedBy = do
    spaces >> string "+" >> spaces
    return FollowedBy

{- SIMPLE SELECTORS FOLLOW -}

-- | selects a tagname followed by one or more secondary selectors
-- example: @ a.foo @, @ *#hello @, @ h1 @ etc
simpleSelectorTag :: ParsecT [Char] u I.Identity Selector
simpleSelectorTag = do
    tagName <- typeSelector <|> universalSelector
    attrs <- secondarySelector <|> return []
    pseudo <- many1 pseudoSelector <|> return []
    return $ Selector tagName attrs pseudo

-- | selects one or more secondary selectors
-- and automatically prepends the universal selector to them.
-- example: @ .foo @, @ #hello @ etc
simpleSelectorNoTag = do
    attrs <- secondarySelector
    pseudo <- many1 pseudoSelector <|> return []
    return $ Selector "*" attrs pseudo

-- | A simple selector is either a type selector or universal selector followed immediately by zero or more attribute selectors, ID selectors, or pseudo-classes, in any order.
simpleSelector :: ParsecT [Char] u I.Identity Selector
simpleSelector = simpleSelectorTag <|> simpleSelectorNoTag <|> try childOf <|> try followedBy <|> space_

-- | One or more simple selectors separated by combinators. 
selector :: ParsecT [Char] u I.Identity [[Selector]]
selector = many1 simpleSelector `sepBy` (spaces >> string "," >> spaces)

css = parse selector ""
