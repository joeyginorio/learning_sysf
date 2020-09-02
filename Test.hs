{-|
Testing.hs
==============================================================================
Tests learning in System F for several terms. Useful for profiling performance.
-}

import Learning
import SystemF

genId :: Context -> Int -> [Term]
genId ctx n =
  let typ = (TyTAbs "X" (TyAbs (TyVar "X") (TyVar "X")))
      in genTerms typ ctx n

lrnId :: Context -> Int -> [Term]
lrnId ctx n =
  let typ = (TyTAbs "X" (TyAbs (TyVar "X") (TyVar "X")))
      exs = [InTy TyBool (InTm TmTrue (Out TmTrue))]
      in learnTerms typ exs ctx n

lrnBoolId :: Context -> Int -> [Term]
lrnBoolId ctx n =
  let typ = TyAbs TyBool TyBool
      exs = [InTm TmTrue (Out TmTrue)]
      in learnTerms typ exs ctx n

lrnProd :: Context -> Int -> [Term]
lrnProd ctx n =
  let typ = TyTAbs "X"
            (TyTAbs "Y"
             (TyTAbs "Z" (TyAbs
                          (TyAbs (TyVar "X")
                           (TyAbs (TyVar "Y")
                            (TyVar "Z")))
                          (TyVar "Z"))))
      body = (TmApp
              (TmApp
                 (TmVar "f")
                 (TmVar "x"))
                (TmVar "y"))
      out = (TmTAbs "Z" (TmAbs "x" TyBool (TmAbs "y" TyBool (TmAbs "f" (TyAbs TyBool(TyAbs TyBool (TyVar "Z"))) body))))
      exs = [InTy TyBool (InTy TyBool (InTm TmTrue (InTm TmTrue (Out out))))]
      in learnTerms typ exs ctx n
      -- in [out]
main = do
  let ctx = []
  let n   = 25
  let typ = (TyTAbs "Z" (TyAbs (TyAbs TyBool (TyAbs TyBool (TyVar "Z"))) (TyVar "Z")))
  let tms = genTerms (TyAbs typ TyBool) [] 30
  print $ tms
