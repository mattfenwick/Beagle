## Here's the full Beagle strategy: string to evaluation ##
------------------------


a. Parsing

 - tokenization:  `String -> [Token]`

   - `Parse.tokenize`

   - token types:  whitespace, comment, symbol, string, open, close

 - whitespace check, no consecutive symbols/strings:  `[Token] -> void` (throws exception)

   - `Parse.checkWhitespace`

 - throw away unneeded tokens:  `[Token] -> [Token]`

   - `Parse.stripTokens`

   - gets rid of whitespace and comment tokens

 - build s-expressions:  `[Token] -> [SExpression]`

   - `Parse.makeSExpressions`

   - s-expression types:  list, symbol, string

 - create Beagle objects:  `SExpression -> BeagleObject`

   - `Reify.makePrimitives`

   - `BeagleObject` types:  `Data.List`, `Data.String`, `Data.Symbol`, `Data.Number`


b. Evaluation

 - macro expansion:  `BeagleObject -> BeagleObject`

   - **not implemented**

 - evaluation:  `BeagleObject -> BeagleObject`

   - `Evaluate.eval`
