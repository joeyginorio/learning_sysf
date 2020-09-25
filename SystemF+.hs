{-|
SystemF+.hs
===============================================================================
Defines syntax, typing, evaluation for System F+. A sugared version of System F
with sum/products + let statements.
-}

import PPrinter
import qualified Data.Map as Map
import qualified Data.Set as Set


{- ========================= Syntax of Terms & Types =========================-}
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
          | TmConstr Constr [Term] Type
          | TmCase Term [(Term, Term)]
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
showTm (TmAbs i typ tm) = parens $ (text "lam ") <+> (text i) <+> (text ":") <+>
                           parens (showTy typ) <+> (text ".") <+>
                           (nest 3 $ line <+> showTm tm)
showTm (TmApp tm1 tm2) = parens $ showTm tm1 <+> text " " <+> showTm tm2
showTm (TmTAbs i tm) = parens $ text "forall " <+> text i <+> text "." <+>
                       showTm tm
showTm (TmTApp tm ty) = parens $ showTm tm <+> text " " <+> showTy ty
showTm (TmLet bs tm) = text "let " <+> (nest 4 $ showTmLet bs <+> line <+>
                       text "in " <+> showTm tm)
showTm (TmConstr c tms ty) = text c <+> text " " <+> sepby " " (map showTm tms)
                             <+> text " as " <+> showTy ty
showTm (TmCase tm tmtms) = text "case " <+> showTm tm <+> text " of" <+>
                           (nest 3 $ line <+> showTmCase tmtms)

showTmLet :: [(Id,Term)] -> Doc
showTmLet [] = nil
showTmLet ((i,tm):[]) = text i <+> text " = " <+> showTm tm
showTmLet ((i,tm):bs) = text i <+> text " = " <+> showTm tm <+> line <+>
                        showTmLet bs

showTmCase :: [(Term, Term)] -> Doc
showTmCase [] = nil
showTmCase ((tm1,tm2):tms) = showTm tm1 <+> text " -> " <+> showTm tm2 <+> line <+>
                             showTmCase tms

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
showTy (TyCase cts) = showTyCase cts

showTyCase :: [(Constr, [Type])] -> Doc
showTyCase [] = nil
showTyCase ((c,ts):[]) = parens (text c <+> text ":" <+> ts')
                         where ts' = sepby "*" (map showTy ts)
showTyCase ((c,ts):cts) = parens (text c <+> text ":" <+> ts') <+> text "+" <+>
                          showTyCase cts
                          where ts' = sepby "*" (map showTy ts)


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
             | ErConstr1 Type
             | ErConstr2 Constr
             | ErConstr3 Constr Type
             | ErCase1
             | ErCase2 Term
             deriving (Eq)

-- For pretty printing errors
instance Show TCError where
  show (ErVar x) = concat ["Variable ", x, " has no binding in the context."]
  show (ErTyVar x) = concat ["Type variable ", x, " has no type."]
  show (ErApp1 trm1 trm2) = concat [show trm2, " is not a valid input to ",
                                   show trm1, "."]
  show (ErApp2 trm) = concat [show trm, " must be an abstraction."]
  show (ErTApp trm) = concat [show trm, " must be a type abstraction."]
  show (ErConstr1 ty) = concat [show ty, " must be a variant type."]
  show (ErConstr2 c) = concat [show c, " has arguments of wrong type."]
  show (ErConstr3 c ty) = concat [show c, " is not a valid constructor for type ",
                                 show ty, "."]
  show (ErCase1) = concat ["Outputs of pattern match must match type."]
  show (ErCase2 tm) = concat ["Incomplete pattern match for ", show tm, "."]

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
typeCheck (TmAbs i ty tm) ctx =
  do ty' <- typeCheck tm ((TmBind i ty):ctx)
     return (TyAbs ty ty')
typeCheck (TmApp tm1 tm2) ctx =
  do ty1 <- typeCheck tm1 ctx
     ty2 <- typeCheck tm2 ctx
     case ty1 of
       (TyAbs ty11 ty12)
         | ty11 == ty2 -> Right $ ty12
         | otherwise -> Left $ ErApp1 tm1 tm2
       _ -> Left $ ErApp2 tm1
typeCheck (TmTAbs i tm) ctx =
  do ty <- typeCheck tm ((TyBind i):ctx)
     return (TyTAbs i ty)
typeCheck (TmTApp tm ty) ctx =
  do ty' <- typeCheck tm ctx
     case ty' of
       (TyTAbs i ty'') -> Right $ subType i ty ty'' freshTyVars
       _                -> Left $ ErTApp tm
typeCheck (TmLet itms tm) ctx =
  let itms' = sequence $ map (\(i,t) -> typeCheck t ctx) itms
      in case itms' of
           (Right []) -> typeCheck tm ctx
           (Right tys) -> typeCheck tm (ctx' ++ ctx)
                          where ctx' = zipWith (\x y -> TmBind (fst x) y) itms tys
           (Left e) -> Left e
typeCheck (TmConstr c tms ty) ctx =
  do tys <- sequence $ map (\tm -> typeCheck tm ctx) tms
     let getArgTys c' ctys' = filter (\x -> fst x == c') ctys'
     case ty of
       (TyCase ctys) -> case getArgTys c ctys of
                          ((_,tys'):[])
                            | tys == tys' -> Right ty
                            | otherwise   -> Left $ ErConstr2 c
                          [] -> Left $ ErConstr3 c ty
       otherwise  -> Left $ ErConstr1 ty
typeCheck (TmCase tm tmtms) ctx =
  do ty <- typeCheck tm ctx
     tm1s <- sequence $ map (\tm -> typeCheck tm ctx) ((fst . unzip) tmtms)
     let ctx1s = map cToContext ((fst . unzip) tmtms)
     let ctx1tm2s = zip ctx1s ((snd . unzip) tmtms)
     tm2s <- sequence $ map (\(ctx1,tm2) -> typeCheck tm2 (ctx ++ ctx1)) ctx1tm2s
     let alleq = all (\x -> x == (tm2s !! 0)) tm2s
     if not alleq then Left $ ErCase1 else
       case ty of
         (TyCase ctys)
           | checkMatch ctys tmtms -> Right (tm2s !! 0)
           | otherwise             -> Left $ ErCase2 tm

cToContext :: Term -> Context
cToContext (TmConstr c tms (TyCase ctys)) =
  let tys = snd ((filter (\x -> fst x == c) ctys) !! 0)
      tmtys = zip tms tys
      in [TmBind i ty | ((TmVar i),ty) <- tmtys]
cToContext _ = []


-- Given a TyCase and TmCase, checks if you have a complete pattern match
checkMatch :: [(Constr, [Type])] -> [(Term, Term)] -> Bool
checkMatch [] [] = True
checkMatch [] tmtms = False
checkMatch ctys [] = False
checkMatch ((c,tys):ctys) (((TmConstr c' _ _),tm):tmtms) =
  if c == c' then True && checkMatch ctys tmtms
  else False
checkMatch ((c,tys):ctys) ((tm1,tm2):tmtms) = False


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
  | x /= i && (Set.member i fvTrm)       = (subTerm x trm rtrm is)
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
