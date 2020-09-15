{-|
SystemF+.hs
===============================================================================
Defines syntax, typing, evaluation for System F+. A sugared version of System F
with sum/products + let statements.
-}

{-
TODO
- syntax
- semantics (typechecker, eval)
-}

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
          | TmCase Id [(Pattern, Term)]
          deriving (Eq)

-- For pretty printing terms
instance Show Term where
  show (TmUnit) = "unit"
  show (TmTrue) = "tt"
  show (TmFalse) = "ff"
  show (TmVar i) = id i
  show (TmAbs i typ trm) = concat ["(", "lam ", i, ":", "(", show typ, ").",
                                   show trm, ")"]
  show (TmApp trm1 trm2) = "(" ++ show trm1 ++ ")" ++ show trm2
  show (TmTAbs i trm) = concat ["(", "forall ", i, ".", show trm, ")"]
  show (TmTApp trm typ) = "(" ++ show trm ++ ")" ++ show typ
  show (TmCase i ps) = concat $ ["case ", show i, " of \n"] ++ [unlines ps']
                       where ps' = map showPTm ps
                             showPTm (p,tm) = "\t" ++ show p ++ " => " ++ show tm

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
  show (TyUnit) = "Unit"
  show (TyBool) = "Bool"
  show (TyVar i) = i
  show (TyAbs typ1 typ2) = concat ["(", show typ1, " -> ", show typ2, ")"]
  show (TyTAbs i typ) = concat ["(", i, ".", show typ, ")"]

-- Syntax of Patterns
data Pattern = PVar Id
             | PConstr Constr [Pattern]
             deriving (Eq)

-- For pretty printing patterns
instance Show Pattern where
  show (PVar i) = ' ':i
  show (PConstr c ps) = concat $ [" (", c] ++ ps' ++ [")"]
                        where ps' = map show ps


