module GoogleSearch where
import HandsomeSoup
import Text.XML.HXT.Core
import Control.Monad

main = do
    doc <- fromUrl "http://www.google.com/search?q=egon+schiele"
    links <- runX $ doc >>> css "h3.r a" ! "href"
    mapM_ putStrLn links
