- validate input
- write tests
- followedby combinator doesn't work yet: how to do siblings in hxt?
- :first-child pseudoclass doesn't work yet: how to do this in hxt?
- Go through this: http://www.w3.org/TR/CSS2/selector.html. Did I miss anything?

# Test cases

    html  body
    html > body
    a.sister
    a#link1
    a.wut#link1
    a[class=sister]
    a[class~=sister]
    a.sister#link2, p
