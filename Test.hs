{-|
Testing.hs
==============================================================================
Tests learning in System F for several terms. Useful for profiling performance.
-}

import Learning
import F
import qualified FPlusParser as FPP
import qualified FPlus as FP
import qualified MParser as MP

parseExs :: String -> [Example]
parseExs s = FP.desugarFExs (fst ((MP.parse (FPP.exs []) s) !! 0))

parseTm :: String -> Term
parseTm s = FP.desugarFTm (fst ((MP.parse (FPP.tm []) s) !! 0))

parseTy :: String -> Type
parseTy s = FP.desugarFTy (fst ((MP.parse (FPP.ty) s) !! 0))

mains =
  let ctx = []
      n   = 20
      ty = parseTy "(X . X -> X -> X) -> (X . X -> X -> X)"
      tms = genTerms ty ctx n
  in tms
