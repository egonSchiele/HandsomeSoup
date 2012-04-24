# HandsomeSoup

Current Status: very very pre-alpha. Usable but buggy.

HandsomeSoup is the library I wish I had when I started parsing HTML in Haskell.

It is built on top of [HXT](http://www.fh-wedel.de/~si/HXmlToolbox/) and adds a few functions that make is easier to work with HTML.

Most importantly, it adds CSS selectors to HXT. The goal of HandsomeSoup is to be a complete CSS2 parser for HXT (it is very close to this right now).

## Install

    cabal install HandsomeSoup

## Example

[Nokogiri](http://nokogiri.org/), the HTML parser for Ruby, has an example showing how to scrape Google search results. This is easy in HandsomeSoup:

    main = do
        doc <- fromUrl "http://www.google.com/search?q=egon+schiele"
        links <- runX $ doc >>> css "h3.r a" ! "href"
        mapM_ putStrLn links

## What can HandsomeSoup do for you?

### Easily parse an online page using `fromUrl`

    doc <- fromUrl "http://example.com"

### Or a local page using `parseHtml`

    contents <- readFile [filename]
    doc <- parseHtml contents

### Easily extract elements using `css`

Here are some valid selectors:

    doc <<< css "a"
    doc <<< css "*"
    doc <<< css "a#link1"
    doc <<< css "a.foo"
    doc <<< css "p > a"
    doc <<< css "#container h1"
    doc <<< css "img[width]"
    doc <<< css "img[width=400]"
    doc <<< css "a[class~=bar]"

### Easily get attributes using `(!)`

    doc <<< css "img" ! "src"
    doc <<< css "a" ! "href"
