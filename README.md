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

To generate polymorphic terms, our examples include types. These types are used to instantiate an example at a particular base type. For example, run the following in ghci to learn at type (forall X.X->X) with examples <Bool,tt,tt>:
```haskell
lrnTerms (TyTAbs "X" (TyAbs (TyVar "X") (TyVar "X"))) [InTy TyBool (InTm TmTrue (Out TmTrue))] [] [] 4
```
This will produce the polymorphic identity function. 
