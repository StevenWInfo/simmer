# Simmer

- In hindsight, I probably should have made "parenthesis" be one of the AST values. It's another "special case" but I have to handle it specially in the parse and interpret code so it probably should be a special case.

## TODO

- Figure out a lot of stuff.
- Not even considering imports yet.
- Probably need some sort of monad for evaluation with the environment (state monad?)
- If conditional can just be shorthand/sugar for a function that takes pairs of functions that are called without parameters, the first being something that returns a boolean to see if the second is evaluated.
- Should make documentation for implementers of the language in other host languages.
- Make a formal grammar.
- Should probably rename all modules to "Simmer.MODULE".
