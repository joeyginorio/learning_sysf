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

line :: Doc
line = 0 `Line` nil

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


{-=========================== UTILITY COMBINATORS ============================-}
text :: String -> Doc
text s = s `Text` nil

parens :: Doc -> Doc
parens x = text "(" <+> x <+> text ")"

angles :: Doc -> Doc
angles x = text "<" <+> x <+> text ">"

sepby :: String -> [Doc] -> Doc
sepby s [] = nil
sepby s (d:[]) = d
sepby s (d:ds) = d <+> text s <+> sepby s ds
