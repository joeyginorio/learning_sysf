{-|
Testing.hs
==============================================================================
Tests learning in System F for several terms. Useful for profiling performance.
-}

import Learning
import SystemF

genId :: Context -> Int -> [Term]
genId ctx n =
  let idTy = (TyTAbs "X" (TyAbs (TyVar "X") (TyVar "X")))
      in genTerms idTy ctx n

main = do
  let ctx = []
  let n   = 20
  let tms = genId ctx n
  print $ tms
