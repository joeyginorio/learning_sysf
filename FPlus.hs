{-|
FPlus.hs
===============================================================================
Defines syntax, typing, evaluation for System F+. A sugared version of System F
with sum/products + let statements.
-}


module FPlus where

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
                          parens (showTy typ) <+> (text ".") <+> showTm tm
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
             | ErCase3
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
  show (ErCase3) = concat ["Arguments to constructors must be variables."]

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
       if not (allVarsC . fst . unzip $ tmtms) then Left $ ErCase3 else
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

allVarsC :: [Term] -> Bool
allVarsC [] = True
allVarsC ((TmConstr _ tms _):cs) = allVars tms && allVarsC cs
allVarsC (c:cs) = False

allVars :: [Term] -> Bool
allVars tms = all (\x -> case x of (TmVar _) -> True; otherwise -> False) tms

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
freeTmVar (TmAbs i _ tm) = (freeTmVar tm) Set.\\ (Set.singleton i)
freeTmVar (TmApp tm1 tm2) = Set.union (freeTmVar tm1) (freeTmVar tm2)
freeTmVar (TmTAbs _ tm) = (freeTmVar tm)
freeTmVar (TmTApp tm _) = (freeTmVar tm)
freeTmVar (TmLet itms tm) = Set.union fvs1 fvs2
  where fvs1 = foldr Set.union Set.empty fvs1'
        fvs1' = map (\(i,t) -> (freeTmVar t) Set.\\ (Set.singleton i)) itms
        fvs2 = freeTmVar tm
freeTmVar (TmConstr _ tms _) = foldr Set.union Set.empty (map freeTmVar tms)
freeTmVar (TmCase tm tmtms) = Set.union fvs1 fvs2
  where fvs1 = freeTmVar tm
        fvs2 = foldr Set.union Set.empty fvs2'
        fvs2' = map (\(tm1,tm2) -> (freeTmVar tm2) Set.\\ (freeTmVar tm1)) tmtms

-- Returns the free variables in a type
freeTyVar :: Type -> Set.Set Id
freeTyVar (TyUnit) = Set.empty
freeTyVar (TyBool) = Set.empty
freeTyVar (TyVar i) = Set.singleton i
freeTyVar (TyAbs typ1 typ2) = Set.union (freeTyVar typ1) (freeTyVar typ2)
freeTyVar (TyTAbs i typ) = (freeTyVar typ) Set.\\ (Set.singleton i)
freeTyVar (TyCase ctys) = foldr Set.union Set.empty fvss
  where tyss = (snd . unzip $ ctys)
        fvss = [foldr Set.union Set.empty (map freeTyVar tys) | tys <- tyss]

-- Replace all instances of a variable with new identifier
replaceTmVar :: Id -> Id -> Term -> Term
replaceTmVar _ _ (TmUnit) = TmUnit
replaceTmVar _ _ (TmTrue) = TmTrue
replaceTmVar _ _ (TmFalse) = TmFalse
replaceTmVar x y (TmVar i)
  | i == x    = TmVar y
  | otherwise = TmVar i
replaceTmVar x y (TmAbs i ty tm)
  | i == x    = TmAbs y ty (replaceTmVar x y tm)
  | otherwise = TmAbs i ty (replaceTmVar x y tm)
replaceTmVar x y (TmApp tm1 tm2) = TmApp tm1' tm2'
  where tm1' = replaceTmVar x y tm1
        tm2' = replaceTmVar x y tm2
replaceTmVar x y (TmTAbs i tm) = TmTAbs i tm'
  where tm' = replaceTmVar x y tm
replaceTmVar x y (TmTApp tm ty) = TmTApp tm' ty
  where tm' = replaceTmVar x y tm
replaceTmVar x y (TmLet itms tm) = TmLet itms' tm'
  where tm' = replaceTmVar x y tm
        itms' = map
                (\(i,t) -> if i == x then (y,replaceTmVar x y t)
                          else (i,replaceTmVar x y t))
                itms
replaceTmVar x y (TmConstr c tms ty) = TmConstr c tms' ty
  where tms' = map (replaceTmVar x y) tms
replaceTmVar x y (TmCase tm tmtms) = TmCase tm' tmtms'
  where tm' = replaceTmVar x y tm
        replaceTmTms = (\(s,t) -> (replaceTmVar x y s, replaceTmVar x y t))
        tmtms' = map replaceTmTms tmtms

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
replaceTyVar x y (TyCase ctys) = TyCase ctys'
  where replaceCTys = (\(c,tys) -> (c, map (replaceTyVar x y) tys))
        ctys' = map replaceCTys ctys

-- Capture-avoiding substitution of terms
subTerm :: Id -> Term -> Term -> [Id] -> Term
subTerm _ _ (TmUnit) _ = TmUnit
subTerm _ _ (TmTrue) _ = TmTrue
subTerm _ _ (TmFalse) _ = TmFalse
subTerm x tm (TmVar i) _
  | x == i    = tm
  | otherwise = (TmVar i)
subTerm x tm t@(TmAbs i ty tm') fvs@(i':is)
  | x == i                               = t
  | x /= i && (not (Set.member i fvTm)) = TmAbs i ty (subTerm x tm tm' fvs)
  | x /= i && (Set.member i fvTm)       = (subTerm x tm rtm is)
  where fvTm = freeTmVar tm
        rtm  = replaceTmVar i i' t
subTerm x tm (TmApp tm1 tm2) fvs = TmApp tm1' tm2'
  where tm1' = subTerm x tm tm1 fvs
        tm2' = subTerm x tm tm2 fvs
subTerm x tm (TmTAbs i tm') fvs = TmTAbs i (subTerm x tm tm' fvs)
subTerm x tm (TmTApp tm' ty) fvs = TmTApp (subTerm x tm tm' fvs) ty
subTerm x tm (TmLet itms tm') fvs =
  let tms = (snd . unzip) itms
      tms' = map (\t -> subTerm x tm t fvs) tms
      fvIs = Set.fromList $ (fst . unzip) itms
      fvTm = freeTmVar tm
      conflicts = Set.toList (Set.intersection fvIs fvTm)
      (fvIs',tm'') = fixLet conflicts (Set.toList fvIs,tm) fvs
      in TmLet (zip fvIs' tms') tm''
subTerm x tm (TmConstr c tms ty) fvs = TmConstr c tms' ty
  where tms' = map (\t -> subTerm x tm t fvs) tms
subTerm x tm (TmCase tm' tmtms) fvs = TmCase tm'' tmtms'
  where tm'' = subTerm x tm tm' fvs
        tmtms' = map (\t -> subMatch x tm t fvs) tmtms

subMatch :: Id -> Term -> (Term, Term) -> [Id] -> (Term, Term)
subMatch x tm (tm1@(TmConstr _ tms _), tm2) fvs =
  let fvC = freeTmVar tm1
      fvTm = freeTmVar tm
      conflicts = Set.toList (Set.intersection fvC fvTm)
      (tm1',tm2') = fixMatch conflicts (tm1,tm2) fvs
      fvTm1 = freeTmVar tm1
      rtm
         | (Set.member x fvTm1) = (tm1,tm2)
         | (not (Set.member x fvTm1))
           && conflicts == [] = (tm1, subTerm x tm tm2 fvs)
         | (not (Set.member x fvTm1))
           && conflicts /= [] = (tm1', subTerm x tm tm2' fvs)
      in rtm

fixMatch :: [Id] -> (Term, Term) -> [Id] -> (Term, Term)
fixMatch [] (tm1,tm2) _ = (tm1,tm2)
fixMatch (x:xs) (tm1,tm2) (i:is) = fixMatch xs (replaceTmVar x i tm1,
                                         replaceTmVar x i tm2) is

fixLet :: [Id] -> ([Id], Term) -> [Id] -> ([Id], Term)
fixLet [] (fvIs, tm) _ = (fvIs, tm)
fixLet (x:xs) (fvIs, tm) (i:is) = fixLet xs (fvIs',tm') is
  where fvIs' = [if x == x' then i else x' | x' <- fvIs]
        tm' = replaceTmVar x i tm

-- Capture-avoiding substitution of types
subType :: Id -> Type -> Type -> [Id] -> Type
subType _ _ (TyUnit) _ = TyUnit
subType _ _ (TyBool) _ = TyBool
subType x ty (TyVar i) _
  | x == i    = ty
  | otherwise = (TyVar i)
subType x ty (TyAbs ty1 ty2) fvs = TyAbs ty1' ty2'
  where ty1' = subType x ty ty1 fvs
        ty2' = subType x ty ty2 fvs
subType x ty t@(TyTAbs i ty') fvs@(i':is)
  | x == i                               = t
  | x /= i && (not (Set.member i fvTy)) = TyTAbs i (subType x ty ty' fvs)
  | x /= i && (Set.member i fvTy)       = TyTAbs i' (subType x ty rty is)
  where fvTy = freeTyVar ty
        rty = replaceTyVar i i' ty'
subType x ty (TyCase ctys) fvs = TyCase ctys'
  where subCTys = (\(c,tys) -> (c, map (\t -> subType x ty t fvs) tys))
        ctys' = map subCTys ctys

-- Capture-avoiding substitution of types in terms
subTypeTerm :: Id -> Type -> Term -> [Id] -> Term
subTypeTerm _ _ (TmUnit) _ = TmUnit
subTypeTerm _ _ (TmTrue) _ = TmTrue
subTypeTerm _ _ (TmFalse) _ = TmFalse
subTypeTerm _ _ tm@(TmVar _) _ = tm
subTypeTerm x ty (TmAbs i ty' tm) fvs = (TmAbs i ty'' tm')
  where ty'' = subType x ty ty' fvs
        tm'  = subTypeTerm x ty tm fvs
subTypeTerm x ty (TmApp tm1 tm2) fvs = (TmApp tm1' tm2')
  where tm1' = subTypeTerm x ty tm1 fvs
        tm2' = subTypeTerm x ty tm2 fvs
subTypeTerm x ty t@(TmTAbs i tm) fvs
  | x == i = t
  | x /= i = TmTAbs i tm'
  where tm' = subTypeTerm x ty tm fvs
subTypeTerm x ty (TmTApp tm ty') fvs = (TmTApp tm' ty'')
  where tm' = subTypeTerm x ty tm fvs
        ty'' = subType x ty ty' fvs
subTypeTerm x ty (TmLet itms tm) fvs = TmLet itms' tm'
  where itms' = map (\(i,t) -> (i,subTypeTerm x ty t fvs)) itms
        tm' = subTypeTerm x ty tm fvs
subTypeTerm x ty (TmConstr c tms ty') fvs = TmConstr c tms' ty''
  where tms' = map (\t -> subTypeTerm x ty t fvs) tms
        ty'' = subType x ty ty' fvs
subTypeTerm x ty (TmCase tm tmtms) fvs = TmCase tm' tmtms'
  where tm' = subTypeTerm x ty tm fvs
        subTyTmTms = (\(s,t) -> (subTypeTerm x ty s fvs, subTypeTerm x ty t fvs))
        tmtms' = map subTyTmTms tmtms

-- Evaluate terms, assuming well-typed
eval :: Term -> Env -> Term
eval (TmUnit) _ = TmUnit
eval (TmTrue) _ = TmTrue
eval (TmFalse) _ = TmFalse
eval tm@(TmVar _) _ = tm
eval tm@(TmAbs _ _ _) _ = tm
eval (TmApp (TmAbs i _ tm1) tm2) (_,fvs) = subTerm i tm2 tm1 fvs
eval (TmApp tm1 tm2) env = eval (TmApp tm1' tm2') env
  where tm1' = eval tm1 env
        tm2' = eval tm2 env
eval tm@(TmTAbs _ _) _ = tm
eval (TmTApp (TmTAbs i tm) ty) (_,fvs) = subTypeTerm i ty tm fvs
eval (TmTApp tm ty) env = eval (TmTApp tm' ty) env
  where tm' = eval tm env
eval (TmLet itms tm) e@(_,fvs) = evalLet itms' tm fvs
  where itms' = map (\(i,t) -> (i, eval t e)) itms
eval (TmConstr c tms ty) env = TmConstr c tms' ty
  where tms' = map (\tm -> eval tm env) tms
eval (TmCase (TmConstr c tms ty) tmtms) (_,fvs) =
  let (TmConstr _ tms' _,tm2) = getMatch c tmtms
      ids = [i | (TmVar i) <- tms']
      idtms = zip ids tms
      in subTerms idtms tm2 fvs

evalLet :: [(Id,Term)] -> Term -> [Id] -> Term
evalLet [] tm _ = tm
evalLet ((i,t):xs) tm fvs = evalLet xs (subTerm i t tm fvs) fvs

getMatch :: Constr -> [(Term,Term)] -> (Term,Term)
getMatch c tmtms = [m | m@((TmConstr c' _ _),_) <- tmtms, c == c'] !! 0

subTerms :: [(Id,Term)] -> Term -> [Id] -> Term
subTerms [] tm _ = tm
subTerms ((i,tm'):itms) tm fvs = subTerms itms (subTerm i tm' tm fvs) fvs
