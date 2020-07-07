The implementation of learning from types is in the function genTerms in learning.hs.
```haskell
genTerms TyBool [] 5
```
That generates all terms of type Bool from the empty context, up to an AST size 5.

The implementation of learning from examples is in the function lrnTerms in learning.hs.
```haskell
lrnTerms TyBool [InTm TmTrue (Out TmTrue)] [] [] 5
```
That generates all terms of type Bool from the empty context, up to an AST size 5 AND which satisfy the example <tt,tt>.
