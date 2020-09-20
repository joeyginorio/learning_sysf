{-|
SystemF+.hs
===============================================================================
Defines syntax, typing, evaluation for System F+. A sugared version of System F
with sum/products + let statements.
-}

import PPrinter

{- ================= Syntax of Terms & Types & Patterns ======================-}
type Id = String
type Constr = String

-- Syntax of Terms
data Term = TmUnit
          | TmTrue
          | TmFalse
          | TmVar Id
          | TmAbs Id Type Term
          | TmApp Term Term
          | TmTAbs Id Term
          | TmTApp Term Type
          | TmLet [(Id,Term)] Term
          | TmCase Id [(Pattern, Term)]
          deriving (Eq)

-- For pretty printing terms
instance Show Term where
  show tm = layout $ showTm tm

  -- Pretty print terms
showTm :: Term -> Doc
showTm (TmUnit) = text "unit"
showTm (TmTrue) = text "tt"
showTm (TmFalse) = text "ff"
showTm (TmVar i) = text i
showTm (TmAbs i typ trm) = parens $ (text "lam ") <+> (text i) <+> (text ":") <+>
                           parens (showTy typ) <+> (text ".") <+>
                           (nest 3 $ line <+> showTm trm)
showTm (TmCase i ps) = text "case " <+> text i <+> text " of" <+>
                       (nest 3 $ line <+> showCase ps)

showCase :: [(Pattern, Term)] -> Doc
showCase [] = nil
showCase ((p,t):pts) = showPn p <+> text " -> " <+> showTm t <+> line <+>
                       showCase pts


-- Syntax of Types
data Type = TyUnit
          | TyBool
          | TyVar Id
          | TyAbs Type Type
          | TyTAbs Id Type
          | TyCase [(Constr, [Type])]
          deriving (Eq)

-- For pretty printing types
instance Show Type where
  show typ = layout $ showTy typ

-- Pretty print types
showTy :: Type -> Doc
showTy (TyUnit) = text "Unit"
showTy (TyBool) = text "Bool"
showTy (TyVar i) = text i
showTy (TyAbs typ1 typ2) = parens $ showTy typ1 <+> text " -> " <+> showTy typ2
showTy (TyTAbs i typ) = parens $ text i <+> text "." <+> showTy typ

-- Syntax of Patterns
data Pattern = PVar Id
             | PConstr Constr [Pattern]
             deriving (Eq)

-- For pretty printing patterns
instance Show Pattern where
  show p = layout $ showPn p

showPn :: Pattern -> Doc
showPn (PVar i) = text i
showPn (PConstr c []) = nil
showPn (PConstr c (p:ps)) = text c <+> text " " <+> showPn p <+>
                            showPn (PConstr c ps)
