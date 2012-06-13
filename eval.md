Problem
-----

When should tokens in the AST be turned into primitive values (such as symbols, strings, numbers, functions)?



Possible solutions
-----

 a. total separation

   1. traverse the s-expression first, turning all tokens into primitives

     - the resulting s-expression has the exact same "shape" as the original

   2. traverse the new s-expression, evaluating it

   - implications:

     - three stages to parsing:
  
       1. tokens -> primitives.  Is this similar to "reader macros" or FEXPRs?

       2. "special" evaluation:  forms such as 'define', 'cond', 'lambda', 'let' ...

       3. "normal" evaluation:  evaluate first element of list to function, apply to rest ...

     - drawbacks:

       1. ???


 b. mixing

   1. 