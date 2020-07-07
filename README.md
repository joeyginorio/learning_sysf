The implementation of learning from types is in the function genTerms in learning.hs. Run the following in ghci:
```haskell
genTerms TyBool [] 5
```
That generates all terms of type Bool from the empty context, up to an AST size 5.

The implementation of learning from examples is in the function lrnTerms in learning.hs. Run the following in ghci:
```haskell
lrnTerms (TyAbs TyBool TyBool) [InTm TmTrue (Out TmTrue)] [] [] 3
```
That generates all terms of type Bool->Bool from the empty context, up to an AST size 3 AND which satisfy the example <tt,tt>.
