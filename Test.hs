{-|
Testing.hs
==============================================================================
Tests learning in System F. Useful for profiling performance.
-}

import Learning
import System.Environment
import qualified FPlusParser as FPP
import qualified FPlus as FP

parseFile' :: String -> IO (Either FP.Term FP.TCError)
parseFile' s = do p <- FPP.parseFile s FPP.prog
                  let p' = FPP.desugarProg p
                  case (FP.typeCheck p' []) of
                    Left e -> return $ Right $ e
                    Right t -> return $ Left $ FP.eval $ FP.learn p'

main = do fName <- getArgs
          case fName of
            [] -> putStrLn "Need command line arg to specify filename."
            (x:xs) -> do p <- parseFile' x
                         print p

