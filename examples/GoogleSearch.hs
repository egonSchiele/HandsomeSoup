module GoogleSearch where
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Control.Monad
import Control.Monad.IO.Class

main = do
    doc <- liftIO $ fromUrl "http://www.google.com/search?q=egon+schiele"
    links <- runX $ doc >>> css "h3.r a" ! "href"
    mapM_ putStrLn links

