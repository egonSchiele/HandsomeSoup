# HandsomeSoup

A collection of utilities for easier HTML parsing with HXT.

## Example

    main = do
        doc <- fromUrl "http://www.google.com/search?q=egon+schiele"
        links <- runX $ doc >>> css "a" ! "href"
        mapM_ putStrLn links

