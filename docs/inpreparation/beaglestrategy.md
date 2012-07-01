## Here's the full Beagle strategy: string to evaluation ##
------------------------


a. Parsing

  1. tokenization:  `String -> [Token]`

    - `Parse.tokenize`

    - token types:  whitespace, comment, symbol, string, open, close

  2. whitespace check, no consecutive symbols/strings:  `[Token] -> void` (throws exception)

    - `Parse.checkWhitespace`

  3. throw away unneeded tokens:  `[Token] -> [Token]`

    - `Parse.stripTokens`

    - gets rid of whitespace and comment tokens

  4. build s-expressions:  `[Token] -> [SExpression]`

    - `Parse.makeSExpressions`

    - s-expression types:  list, symbol, string

  5. create Beagle objects:  `SExpression -> BeagleObject`

    - `Reify.makePrimitives`

    - `BeagleObject` types:  `Data.List`, `Data.String`, `Data.Symbol`, `Data.Number`


b. Evaluation

  6. macro expansion:  `BeagleObject -> BeagleObject`

    - **not implemented**

  7. evaluation:  `BeagleObject -> BeagleObject`

    - `Evaluate.eval`
