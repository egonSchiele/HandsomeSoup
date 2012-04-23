- remove trace statements
- validate input
- write tests
- break out css parser into it's own library
- refactor code
- write haddock docs
- followedby combinator doesn't work yet: how to do siblings in hxt?
- :first-child pseudoclass doesn't work yet: how to do this in hxt?


# Test cases

html  body
html > body
a.sister
a#link1
a.wut#link1
a[class=sister]
a[class~=sister]
a.sister#link2, p
