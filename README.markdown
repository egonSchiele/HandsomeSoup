# HandsomeSoup

Current Status: Usable and stable. **Needs GHC 7.6**. Please file bugs!

HandsomeSoup is the library I wish I had when I started parsing HTML in Haskell.

It is built on top of [HXT](http://www.fh-wedel.de/~si/HXmlToolbox/) and adds a few functions that make it easier to work with HTML.

Most importantly, it adds CSS selectors to HXT. The goal of HandsomeSoup is to be a complete CSS2 selector parser for HXT.

## Install

    cabal install HandsomeSoup

## Example

[Nokogiri](http://nokogiri.org/), the HTML parser for Ruby, has an example showing how to scrape Google search results. This is easy in HandsomeSoup:

    import Text.XML.HXT.Core
    import Text.HandsomeSoup
    
    main = do
        let doc = fromUrl "http://www.google.com/search?q=egon+schiele"
        links <- runX $ doc >>> css "h3.r a" ! "href"
        mapM_ putStrLn links

## What can HandsomeSoup do for you?

### Easily parse an online page using `fromUrl`

    let doc = fromUrl "http://example.com"

### Or a local page using `parseHtml`

    contents <- readFile [filename]
    let doc = parseHtml contents

### Easily extract elements using `css`

Here are some valid selectors:

    doc <<< css "a"
    doc <<< css "*"
    doc <<< css "a#link1"
    doc <<< css "a.foo"
    doc <<< css "p > a"
    doc <<< css "p strong"
    doc <<< css "#container h1"
    doc <<< css "img[width]"
    doc <<< css "img[width=400]"
    doc <<< css "a[class~=bar]"
    doc <<< css "a:first-child"

### Easily get attributes using `(!)`

    doc <<< css "img" ! "src"
    doc <<< css "a" ! "href"

## Docs

Find [Haddock docs on Hackage](http://hackage.haskell.org/package/HandsomeSoup).

I also wrote [The Complete Guide To Parsing HXT With Haskell](http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html).

## Credits

Made by [Adit](http://adit.io).
