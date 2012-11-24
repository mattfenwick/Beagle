## Special form names and their interaction with regular namespaces ##

with special forms now being all special, there's some weird stuff going on with names:

    {define define 3}

this code snippet works, because the first `define` is a special form (indicated by the
curly brackets), while the second one is a symbol

if you do:

    cond

in a fresh interpreter, it will tell you that `cond` is undefined, even though this works:

    {cond [] 3}

possible solutions:

 1. make all the special form names un-define-able and un-set!-able.
    downsides:  breaks uniformity, breaks forwards compatibility, is ugly

 2. put special form names in the environment.
    downsides:  need a runtime data type for special forms; the symbols
    will be subject to shadowing and redefining.  

 3. make special form names tokenize differently, to be classified as some
    other kind of token than a symbol.
    downsides:  forwards compatibility, breaks uniformity by introducing
    an artificial divide between some names and other names.  probably
    my least favorite solution

## Newlines ##

Currently, the comment parser is the only one for which the newline character matters; 
comments end at `\n`.  So if the newline characters aren't `\n`, the first comment will
presumably consume the entire rest of the file.

Possible solutions:

 1. allow all possible newline sequences to end a comment

