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

{- ====================== Syntax of Terms & Types  ==========================-}
type Id = String
type Constr = String

data Term = TmUnit
          | TmTrue
          | TmFalse
          | TmVar Id
          | TmAbs Id Type Term
          | TmApp Term Term
          | TmTAbs Id Term
          | TmTApp Term Type
          | TmFst Term
          | TmSnd Term
          | TmProd Term Term
          | TmInL Term
          | TmInR Term
          | TmSCase Term (Id,Term) (Id,Term)
          | TmVCase Id [(Pattern, Term)]
          deriving (Eq, Show)

data Type = TyUnit
          | TyBool
          | TyAbs Type Type
          | TyTAbs Id Type
          | TyProd Type Type
          | TySum Type Type
          | TyVariant [(Constr, [Type])]
          deriving (Eq, Show)

data Pattern = 


