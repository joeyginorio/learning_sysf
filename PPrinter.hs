{-|
PPrinter.hs
===============================================================================
Pretty printing based off Wadler's "Prettier printer" paper.
-}

module PPrinter where 

{-============================ CUSTOM INFIX OPS ==============================-}
infixr 6 <+>

{-============================= DOC DATATYPE =================================-}
data Doc = Nil
         | String `Text` Doc
         | Int `Line` Doc
         deriving (Show)

{-=========================== CORE COMBINATORS ===============================-}
nil :: Doc
nil = Nil

(<+>) :: Doc -> Doc -> Doc
(s `Text` x) <+> y = s `Text` (x <+> y)
(i `Line` x) <+> y = i `Line` (x <+> y)
(Nil) <+> y        = y

nest :: Int -> Doc -> Doc
nest i (s `Text` x) = s `Text` nest i x
nest i (j `Line` x) = (i+j) `Line` nest i x
nest i (Nil) = Nil

layout :: Doc -> String
layout (s `Text` x) = s ++ layout x
layout (i `Line` x) = '\n': replicate i ' ' ++ layout x
layout (Nil)        = ""
