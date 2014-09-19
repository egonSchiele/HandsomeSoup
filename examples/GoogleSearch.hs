module Main where

import Text.HandsomeSoup
import Text.XML.HXT.Core

main :: IO ()
main = do
    let doc = fromUrl "http://www.google.com/search?q=egon+schiele"
    links <- runX $ doc >>> css "h3.r a" ! "href"
    mapM_ putStrLn links
