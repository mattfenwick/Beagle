## Newlines ##

Currently, the comment parser is the only one for which the newline character matters; 
comments end at `\n`.  So if the newline characters aren't `\n`, the first comment will
presumably consume the entire rest of the file.

Possible solutions:

 1. allow all possible newline sequences to end a comment

