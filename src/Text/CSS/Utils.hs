module Text.CSS.Utils where
import Data.List

-- like break, except don't keep the element you broke on.
-- and it takes a list as the thing to break on.
splitOn a xs = _splitOn a "" xs

_splitOn _ begin [] = (begin, [])
_splitOn a begin end@(x:xs)
  | a `isPrefixOf` end = (begin, drop (length a) end)
  | otherwise = _splitOn a (begin ++ [x]) xs

