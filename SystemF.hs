{-|
SystemF.hs
==============================================================================
Defines syntax, typing, evaluation for System F. Follows the specification in
Chapter 2 of "Learning in System F".
-}

module SystemF where

import qualified Data.Map as Map
import qualified Data.Set as Set


{- ====================== Syntax of Terms & Types  ==========================-}
type Id = String

data Term = TmUnit
          | TmTrue
          | TmFalse
          | TmVar Id
          | TmAbs Id Type Term
          | TmApp Term Term
          | TmTAbs Id Term
          | TmTApp Term Type
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

data Type = TyUnit
          | TyBool
          | TyVar Id
          | TyAbs Type Type
          | TyTAbs Id Type
          deriving (Eq)

-- For pretty printing types
instance Show Type where
  show (TyUnit) = "Unit"
  show (TyBool) = "Bool"
  show (TyVar i) = id i
  show (TyAbs typ1 typ2) = concat ["(", show typ1, " -> ", show typ2, ")"]
  show (TyTAbs i typ) = concat ["(", i, ".", show typ, ")"]


{- =============================== Typing  =================================-}

data Binding = TmBind Id Type
             | TyBind Id
             deriving (Eq, Show)

type Context = [Binding]

-- Different kinds of typechecking errors
data TCError = ErVar Id
             | ErTyVar Id
             | ErApp1 Term Term
             | ErApp2 Term
             | ErTApp Term
             deriving (Eq)

-- For pretty printing errors
instance Show TCError where
  show (ErVar x) = concat ["Variable ", x, " has no binding in the context."]
  show (ErTyVar x) = concat ["Type variable ", x, " has no type."]
  show (ErApp1 trm1 trm2) = concat [show trm2, " is not a valid input to ",
                                   show trm1, "."]
  show (ErApp2 trm) = concat [show trm, " must be an abstraction."]
  show (ErTApp trm) = concat [show trm, " must be a type abstraction."]

-- Extract id from a binding
idFromBinding :: Binding -> Id
idFromBinding (TmBind i _) = i
idFromBinding (TyBind i) = i

-- Extract a type, if applicable, from a binding
typeFromBinding :: Binding -> Either TCError Type
typeFromBinding (TmBind i typ) = Right typ
typeFromBinding (TyBind i) = Left $ ErTyVar i

-- Extract a type, if applicable, from a context
typeFromContext :: Id -> Context -> Either TCError Type
typeFromContext i [] = Left $ ErVar i
typeFromContext i ctx = case ctx' of
  [] -> Left (ErVar i)
  _ -> typeFromBinding (head ctx')
  where ctx' = Prelude.filter (\x -> idFromBinding x == i) ctx

-- Typecheck terms, with monadic error handling
typeCheck :: Term -> Context -> Either TCError Type
typeCheck (TmUnit) _ = Right TyUnit
typeCheck (TmTrue) _ = Right TyBool
typeCheck (TmFalse) _ = Right TyBool
typeCheck (TmVar i) ctx = typeFromContext i ctx
typeCheck (TmAbs i typ trm) ctx = do
  typ' <- typeCheck trm ((TmBind i typ):ctx)
  return (TyAbs typ typ')
typeCheck (TmApp trm1 trm2) ctx = do
  typ1 <- typeCheck trm1 ctx
  typ2 <- typeCheck trm2 ctx
  case typ1 of
    (TyAbs typ11 typ12)
      | typ11 == typ2 -> Right $ typ12
      | otherwise -> Left $ ErApp1 trm1 trm2
    _ -> Left $ ErApp2 trm1
typeCheck (TmTAbs i trm) ctx = do
  typ <- typeCheck trm ((TyBind i):ctx)
  return (TyTAbs i typ)
typeCheck (TmTApp trm typ) ctx = do
  typ' <- typeCheck trm ctx
  case typ' of
    (TyTAbs i typ'') -> Right $ subType i typ typ'' freshTyVars
    _                -> Left $ ErTApp trm


{- =============================== Evaluation  =================================-}

-- Fresh variables for capture-avoiding substitution
freshTmVars :: [Id]
freshTmVars = genFresh (repeat "$x") [1..]

freshTyVars :: [Id]
freshTyVars = genFresh (repeat "$X") [1..]

genFresh :: [Id] -> [Int] -> [Id]
genFresh (x:xs) (y:ys) = (x ++ (show y)) : genFresh xs ys


-- Environment holds term/type bindings + infinite list of fresh variables
type Env = (Map.Map Id (Either Term Type), [Id])

-- Returns the free variables in a term
freeTmVar :: Term -> Set.Set Id
freeTmVar (TmUnit) = Set.empty
freeTmVar (TmTrue) = Set.empty
freeTmVar (TmFalse) = Set.empty
freeTmVar (TmVar i) = Set.singleton i
freeTmVar (TmAbs i _ trm) = (freeTmVar trm) Set.\\ (Set.singleton i)
freeTmVar (TmApp trm1 trm2) = Set.union (freeTmVar trm1) (freeTmVar trm2)
freeTmVar (TmTAbs _ trm) = (freeTmVar trm)
freeTmVar (TmTApp trm _) = (freeTmVar trm)

-- Returns the free variables in a type
freeTyVar :: Type -> Set.Set Id
freeTyVar (TyUnit) = Set.empty
freeTyVar (TyBool) = Set.empty
freeTyVar (TyVar i) = Set.singleton i
freeTyVar (TyAbs typ1 typ2) = Set.union (freeTyVar typ1) (freeTyVar typ2)
freeTyVar (TyTAbs i typ) = (freeTyVar typ) Set.\\ (Set.singleton i)

-- Replace all instances of a variable with new identifier
replaceTmVar :: Id -> Id -> Term -> Term
replaceTmVar _ _ (TmUnit) = TmUnit
replaceTmVar _ _ (TmTrue) = TmTrue
replaceTmVar _ _ (TmFalse) = TmFalse
replaceTmVar x y (TmVar i)
  | i == x    = TmVar y
  | otherwise = TmVar i
replaceTmVar x y (TmAbs i typ trm)
  | i == x    = TmAbs y typ (replaceTmVar x y trm)
  | otherwise = TmAbs i typ (replaceTmVar x y trm)
replaceTmVar x y (TmApp trm1 trm2) = TmApp trm1' trm2'
  where trm1' = replaceTmVar x y trm1
        trm2' = replaceTmVar x y trm2
replaceTmVar x y (TmTAbs i trm) = TmTAbs i trm'
  where trm' = replaceTmVar x y trm
replaceTmVar x y (TmTApp trm typ) = TmTApp trm' typ
  where trm' = replaceTmVar x y trm

-- Replace all instances of a type variable with new identifier
replaceTyVar :: Id -> Id -> Type -> Type
replaceTyVar _ _ (TyUnit) = TyUnit
replaceTyVar _ _ (TyBool) = TyBool
replaceTyVar x y (TyVar i)
  | i == x    = TyVar y
  | otherwise = TyVar i
replaceTyVar x y (TyAbs typ1 typ2) = TyAbs typ1' typ2'
  where typ1' = replaceTyVar x y typ1
        typ2' = replaceTyVar x y typ2
replaceTyVar x y (TyTAbs i typ)
  | i == x     = TyTAbs y (replaceTyVar x y typ)
  | otherwise  = TyTAbs i (replaceTyVar x y typ)

-- Capture-avoiding substitution of terms
subTerm :: Id -> Term -> Term -> [Id] -> Term
subTerm _ _ (TmUnit) _ = TmUnit
subTerm _ _ (TmTrue) _ = TmTrue
subTerm _ _ (TmFalse) _ = TmFalse
subTerm x trm (TmVar i) _
  | x == i    = trm
  | otherwise = (TmVar i)
subTerm x trm t@(TmAbs i typ trm') fvs@(i':is)
  | x == i                               = t
  | x /= i && (not (Set.member i fvTrm)) = TmAbs i typ (subTerm x trm trm' fvs)
  | x /= i && (Set.member i fvTrm)       = TmAbs i' typ (subTerm x trm rtrm is)
  where fvTrm = freeTmVar trm
        rtrm  = replaceTmVar i i' t
subTerm x trm (TmApp trm1 trm2) fvs = TmApp trm1' trm2'
  where trm1' = subTerm x trm trm1 fvs
        trm2' = subTerm x trm trm2 fvs
subTerm x trm (TmTAbs i trm') fvs = TmTAbs i (subTerm x trm trm' fvs)
subTerm x trm (TmTApp trm' typ) fvs = TmTApp (subTerm x trm trm' fvs) typ

-- Capture-avoiding substitution of types
subType :: Id -> Type -> Type -> [Id] -> Type
subType _ _ (TyUnit) _ = TyUnit
subType _ _ (TyBool) _ = TyBool
subType x typ (TyVar i) _
  | x == i    = typ
  | otherwise = (TyVar i)
subType x typ (TyAbs typ1 typ2) fvs = TyAbs typ1' typ2'
  where typ1' = subType x typ typ1 fvs
        typ2' = subType x typ typ2 fvs
subType x typ t@(TyTAbs i typ') fvs@(i':is)
  | x == i                               = t
  | x /= i && (not (Set.member i fvTyp)) = TyTAbs i (subType x typ typ' fvs)
  | x /= i && (Set.member i fvTyp)       = TyTAbs i' (subType x typ rtyp is)
  where fvTyp = freeTyVar typ
        rtyp = replaceTyVar i i' typ'

-- Capture-avoiding substitution of types in terms
subTypeTerm :: Id -> Type -> Term -> [Id] -> Term
subTypeTerm _ _ (TmUnit) _ = TmUnit
subTypeTerm _ _ (TmTrue) _ = TmTrue
subTypeTerm _ _ (TmFalse) _ = TmFalse
subTypeTerm _ _ trm@(TmVar _) _ = trm
subTypeTerm x typ (TmAbs i typ' trm) fvs = (TmAbs i typ'' trm')
  where typ'' = subType x typ typ' fvs
        trm'  = subTypeTerm x typ trm fvs
subTypeTerm x typ (TmApp trm1 trm2) fvs = (TmApp trm1' trm2')
  where trm1' = subTypeTerm x typ trm1 fvs
        trm2' = subTypeTerm x typ trm2 fvs
subTypeTerm x typ t@(TmTAbs i trm) fvs
  | x == i = t
  | x /= i = TmTAbs i trm'
  where trm' = subTypeTerm x typ trm fvs

-- Evaluate terms, assuming well-typed
eval :: Term -> Env -> Term
eval (TmUnit) _ = TmUnit
eval (TmTrue) _ = TmTrue
eval (TmFalse) _ = TmFalse
eval trm@(TmVar _) _ = trm
eval trm@(TmAbs _ _ _) _ = trm
eval (TmApp (TmAbs i _ trm1) trm2) (_,fvs) = subTerm i trm2 trm1 fvs
eval (TmApp trm1 trm2) env = eval (TmApp trm1' trm2') env
  where trm1' = eval trm1 env
        trm2' = eval trm2 env
eval trm@(TmTAbs _ _) _ = trm
eval (TmTApp (TmTAbs i trm) typ) (_,fvs) = subTypeTerm i typ trm fvs
eval (TmTApp trm typ) env = eval (TmTApp trm' typ) env
  where trm' = eval trm env
