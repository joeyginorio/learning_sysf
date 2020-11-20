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
import qualified Learning as L
import qualified F as F

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
          | TmLSpec [Example] Type
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
showTm (TmLSpec xs ty) = parens $ ((text (show xs)) <+> text " :: " <+> showTy ty)

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


data Example = Out Term
             | InTm Term Example
             | InTy Type Example
             deriving (Eq)

instance Show Example where
  show e =
    let show' (Out t) = "tm: " ++ show t
        show' (InTm t e) = "tm: " ++ show t ++ "," ++ show' e
        show' (InTy t e) = "ty: " ++ show t ++ "," ++ show' e
        in "<" ++ show' e ++ ">"



{- =============================== Typing  =================================-}

data Binding = TmBind Id Type
             | TyBind Id
             deriving (Eq, Show)

type Context = [Binding]

-- Different kinds of typechecking errors
data TCError = ErVar Id
             | ErTyVar Id
             | ErApp1 Term Term Type Type
             | ErApp2 Term
             | ErTApp Term
             | ErConstr1 Type
             | ErConstr2 Constr
             | ErConstr3 Constr Type
             | ErCase1
             | ErCase2 Term
             | ErCase3
             | ErExample
             deriving (Eq)

-- For pretty printing errors
instance Show TCError where
  show (ErVar x) = concat ["Variable ", x, " has no binding in the context."]
  show (ErTyVar x) = concat ["Type variable ", x, " has no type."]
  show (ErApp1 trm1 trm2 ty1 ty2) =
    concat [show trm2, " is not a valid input to ", show trm1, ". ",
           show ty2, " does not plug into ", show ty1, "."]
  show (ErApp2 trm) = concat [show trm, " must be an abstraction."]
  show (ErTApp trm) = concat [show trm, " must be a type abstraction."]
  show (ErConstr1 ty) = concat [show ty, " must be a variant type."]
  show (ErConstr2 c) = concat [show c, " has arguments of wrong type."]
  show (ErConstr3 c ty) = concat [show c, " is not a valid constructor for type ",
                                 show ty, "."]
  show (ErCase1) = concat ["Outputs of pattern match must match type."]
  show (ErCase2 tm) = concat ["Incomplete pattern match for ", show tm, "."]
  show (ErCase3) = concat ["Arguments to constructors must be variables."]
  show (ErExample) = concat ["Examples do not typecheck."]

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
         | (desugarTy ty11) == (desugarTy ty2) -> Right $ ty12
         | otherwise -> Left $ ErApp1 tm1 tm2 ty1 ty2
       _ -> Left $ ErApp2 tm1
typeCheck (TmTAbs i tm) ctx =
  do ty <- typeCheck tm ((TyBind i):ctx)
     return (TyTAbs i ty)
typeCheck (TmTApp tm ty) ctx =
  do ty' <- typeCheck tm ctx
     case ty' of
       (TyTAbs i ty'') -> Right $ subType i ty ty'' freshTyVars
       _                -> Left $ ErTApp tm
typeCheck (TmLet itms tm) ctx = tcLet itms tm ctx
typeCheck (TmConstr c tms ty) ctx =
  do tys <- sequence $ map (\tm -> typeCheck tm ctx) tms
     let getArgTys c' ctys' = filter (\x -> fst x == c') ctys'
     case ty of
       (TyCase ctys) -> case getArgTys c ctys of
                          ((_,tys'):[])
                            | tys == tys'' -> Right ty
                            | otherwise   -> Left $ ErConstr2 c
                            where tys'' = map (\x -> if x == (TyVar "#R")
                                                     then TyTAbs "#R" ty
                                                     else x) tys'
                          [] -> Left $ ErConstr3 c ty
       otherwise  -> Left $ ErConstr1 ty
typeCheck (TmCase tm tmtms) ctx = typeCheck (desugarCase tm tmtms) ctx'
  where ctx' = desugarCtx ctx
typeCheck (TmLSpec exs ty) ctx = tcExamples exs ty ctx

tcExamples :: [Example] -> Type -> Context -> Either TCError Type
tcExamples exs tyt ctx =
  do tys <- sequence $ map (\x -> tcExample x tyt tyt ctx) exs
     return $ tys !! 0

tcExample :: Example -> Type -> Type -> Context -> Either TCError Type
tcExample (Out tm) tyr tyt ctx = do ty' <- typeCheck tm ctx
                                    if ty' `betaEqualTy` tyr
                                      then Right tyt
                                      else Left ErExample
tcExample (InTm tm ex) (TyAbs ty1 ty2) tyt ctx =
  do ty' <- typeCheck tm ctx
     if ty' `betaEqualTy` ty1
       then (tcExample ex ty2 tyt ctx)
       else Left ErExample
tcExample (InTy ty ex) (TyTAbs i ty') tyt ctx = tcExample ex ty'' tyt ctx
  where ty'' = subType i ty ty' freshTyVars
tcExample e _ _ _ = Left ErExample

-- Beta equality of types
betaEqualTy :: Type -> Type -> Bool
betaEqualTy ty1 ty2 = betaEqualTy' (desugarTy ty1) (desugarTy ty2) freshTyVars

betaEqualTy' :: Type -> Type -> [Id] -> Bool
betaEqualTy' ty1@(TyTAbs i1 typ1) ty2@(TyTAbs i2 typ2) (i:is) =
  let (TyTAbs _ typ1') = replaceTyVar i1 i ty1
      (TyTAbs _ typ2') = replaceTyVar i2 i ty2
      in betaEqualTy' typ1' typ2' is
betaEqualTy' typ1 typ2 _ = typ1 == typ2


desugarCtx :: Context -> Context
desugarCtx [] = []
desugarCtx (b@(TmBind i t):ctx) = (TmBind i (desugarTy t)):(desugarCtx ctx)
desugarCtx (b@(TyBind i):ctx) = b:(desugarCtx ctx)

desugarFCtx :: Context -> F.Context
desugarFCtx [] = []
desugarFCtx (b@(TmBind i t):ctx) = (F.TmBind i (desugarFTy t)):(desugarFCtx ctx)
desugarFCtx (b@(TyBind i):ctx) = (F.TyBind i):(desugarFCtx ctx)

tcLet :: [(Id,Term)] -> Term -> Context -> Either TCError Type
tcLet [] tm' ctx = typeCheck tm' ctx
tcLet ((i,tm):itms) tm' ctx = do t <- typeCheck tm ctx
                                 tcLet itms tm' ((TmBind i t):ctx)

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
allVars (TmUnit:[]) = True
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
subTypeTerm x ty (TmLSpec exs ty') fvs = TmLSpec exs' ty''
  where exs' = subTypeExs x ty exs fvs
        ty'' = subType x ty ty' fvs

subTypeExs :: Id -> Type -> [Example] -> [Id] -> [Example]
subTypeExs x ty [] fvs = []
subTypeExs x ty (ex:exs) fvs = ex':exs'
  where ex' = subTypeEx x ty ex fvs
        exs' = subTypeExs x ty exs fvs

subTypeEx :: Id -> Type -> Example -> [Id] -> Example
subTypeEx x ty (Out tm) fvs = Out (subTypeTerm x ty tm fvs)
subTypeEx x ty (InTm tm ex) fvs = (InTm tm' ex')
  where tm' = subTypeTerm x ty tm fvs
        ex' = subTypeEx x ty ex fvs
subTypeEx x ty (InTy ty' ex) fvs = (InTy ty'' ex')
  where ty'' = subType x ty ty' fvs
        ex' = subTypeEx x ty ex fvs



-- Evaluate terms, assuming well-typed
eval :: Term -> Term
eval tm = eval' tm' (Map.empty,freshTmVars)
  where tm' = desugarTm tm

eval' :: Term -> Env -> Term
eval' (TmUnit) _ = TmUnit
eval' (TmTrue) _ = TmTrue
eval' (TmFalse) _ = TmFalse
eval' tm@(TmVar i) (m,fvs) = val
  where (Left val) = Map.findWithDefault (Left tm) i m
eval' (TmAbs i ty tm) env = TmAbs i ty (eval' tm env)
eval' (TmApp (TmAbs i _ tm1) tm2) e@(_,fvs) = eval' (subTerm i tm2 tm1 fvs) e
-- eval' (TmApp tm1 tm2) env = eval' (TmApp tm1' tm2') env
  -- where tm1' = eval' tm1 env
        -- tm2' = eval' tm2 env
eval' (TmApp tm1 tm2) env = case (eval' tm1 env) of
                              t@(TmAbs _ _ _) -> eval' (TmApp t (eval' tm2 env)) env
                              otherwise       -> TmApp (eval' tm1 env) (eval' tm2 env)
eval' (TmTAbs i tm) env = (TmTAbs i (eval' tm env))
eval' (TmTApp (TmTAbs i tm) ty) e@(_,fvs) = eval' (subTypeTerm i ty tm fvs) e
eval' (TmTApp (TmVar i) ty) e@(_,fvs) = TmTApp (eval' (TmVar i) e) ty
eval' (TmTApp tm ty) env = case (eval' tm env) of
                             t@(TmTAbs _ _) -> eval' (TmTApp t ty) env
                             otherwise      -> TmTApp (eval' tm env) ty
eval' (TmLet itms tm) e@(_,fvs) = evalLet itms tm e
eval' (TmConstr c tms ty) env = TmConstr c tms' ty
  where tms' = map (\tm -> eval' tm env) tms
eval' (TmCase tm@(TmTApp (TmTAbs _ (TmConstr _ _ _)) _) tmtms) e@(_,fvs) =
  eval' (desugarCase tm tmtms) e
eval' (TmCase (TmTApp tm ty) tmtms) e = eval' (TmCase (TmTApp tm' ty) tmtms) e
  where tm' = eval' tm e
eval' t e = t

evalLet :: [(Id,Term)] -> Term -> Env -> Term
evalLet [] tm e = eval' tm e
evalLet ((i,t):xs) tm e@(_,fvs) = evalLet xs' tm' e
  where tm' = subTerm i t tm fvs
        xs' = map (\(i',t') -> (i',subTerm i t t' fvs)) xs

getMatch :: Constr -> [(Term,Term)] -> (Term,Term)
getMatch c tmtms = [m | m@((TmConstr c' _ _),_) <- tmtms, c == c'] !! 0

subTerms :: [(Id,Term)] -> Term -> [Id] -> Term
subTerms [] tm _ = tm
subTerms ((i,tm'):itms) tm fvs = subTerms itms (subTerm i tm' tm fvs) fvs


{- =============================== Desugaring  =================================-}

desugarTy :: Type -> Type
desugarTy (TyUnit) = TyUnit
desugarTy (TyBool) = TyBool
desugarTy (TyVar i) = TyVar i
desugarTy (TyAbs ty1 ty2) = TyAbs ty1' ty2'
  where ty1' = desugarTy ty1
        ty2' = desugarTy ty2
desugarTy (TyCase ctys) = desugarTyCase ctys
desugarTy (TyTAbs i ty) = (TyTAbs i (desugarTy ty))

desugarTyCase :: [(Constr, [Type])] -> Type
desugarTyCase ctys = ty1
  where ty1 = foldr (\x a -> TyAbs x a) (TyVar "#R") ty1'
        ty1' = desugarConstr ((snd . unzip) ctys)
        desugarConstr = map (foldr (\x a -> TyAbs (desugarTy x) a) (TyVar "#R"))

desugarTyCase1 :: [(Constr, [Type])] -> Type
desugarTyCase1 ctys = ty1
  where ty1 = foldr (\x a -> TyAbs x a) (TyVar "#R") ty1'
        ty1' = desugarConstr ((snd . unzip) ctys)
        desugarConstr = map (foldr (\x a -> TyAbs x a) (TyVar "#R"))

desugarTm :: Term -> Term
desugarTm (TmAbs i ty tm) = TmAbs i ty' tm'
  where ty' = desugarTy ty
        tm' = desugarTm tm
desugarTm (TmApp tm1 tm2) = TmApp tm1' tm2'
  where tm1' = desugarTm tm1
        tm2' = desugarTm tm2
desugarTm (TmTAbs i tm) = TmTAbs i tm'
  where tm' = desugarTm tm
desugarTm (TmTApp tm ty) = TmTApp tm' ty'
  where tm' = desugarTm tm
        ty' = desugarTy ty
desugarTm (TmLet itms tm) = foldr f tm' itms
  where tm' = desugarTm tm
        right = (\(Right x) -> x)
        typeCheck' = (\t -> desugarTy (right (typeCheck t [])))
        f = (\(i,t) a -> TmApp (TmAbs i (typeCheck' t) a) (desugarTm t))
desugarTm (TmConstr c tms ty) = desugarConstr c tms ty
desugarTm (TmCase tm tmtms) = desugarCase tm tmtms
desugarTm tm = tm

desugarExs :: [Example] -> [Example]
desugarExs exs = map desugarEx exs

desugarEx :: Example -> Example
desugarEx (Out tm) = (Out (desugarTm tm))
desugarEx (InTm tm ex) = (InTm (desugarTm tm) (desugarEx ex))
desugarEx (InTy ty ex) = (InTy (desugarTy ty) (desugarEx ex))

desugarFExs :: [Example] -> [L.Example]
desugarFExs exs = map desugarFEx exs

desugarFEx :: Example -> L.Example
desugarFEx (Out tm) = (L.Out (desugarFTm tm))
desugarFEx (InTm tm ex) = (L.InTm (desugarFTm tm) (desugarFEx ex))
desugarFEx (InTy ty ex) = (L.InTy (desugarFTy ty) (desugarFEx ex))

desugarConstr :: Constr -> [Term] -> Type -> Term
desugarConstr c tms ty@(TyCase ctys) = foldl (\a t -> TmApp a (desugarTm t)) tm tms
  where tyas = snd $ (filter (\(c',_) -> if c == c' then True else False) ctys) !! 0
        tyas' = map (\x -> if x == (TyVar "#R")
                           then TyTAbs "#R" (desugarTy ty)
                           else x) tyas
        as = ["a" ++ show i | i <- [0..(length tyas - 1)]]
        tycs = getTycs ctys
        cs = ["c" ++ show i | i <- [0..(length tycs - 1)]]
        tm = foldr (\(i,t) a -> TmAbs i (desugarTy t) a) tm' (zip as tyas')
        tm' = TmTAbs "#R" tm''
        tm'' = foldr (\(i,t) a -> TmAbs i (desugarTy t) a) tm''' (zip cs tycs)
        ci = getCi c ctys
        as' = map TmVar as
        cs' = map TmVar cs
        as'' = map (\(a@(TmVar i), t) -> if t == (TyVar "#R")
                                         then foldl1
                                              TmApp
                                              ((TmTApp a (TyVar "#R")):cs')
                                         else a) (zip as' tyas)
        tm''' = foldl1 TmApp ((TmVar (cs !! ci)):as'')

getTycs :: [(Constr,[Type])] -> [Type]
getTycs [] = []
getTycs ((c,tys):ctys) = ty : (getTycs ctys)
  where ty = foldr (\x a -> TyAbs x a) (TyVar "#R") tys

getCi :: Constr -> [(Constr, [Type])] -> Int
getCi c [] = 0
getCi c ((c',tys):ctys) = if c == c' then 0 else 1 + getCi c ctys

desugarCase :: Term -> [(Term,Term)] -> Term
desugarCase (TmTApp (TmTAbs "#R" (TmConstr c tms tyc)) rty) tmtms =
  let tm = desugarConstr c tms tyc
      cctms = desugarCCases tmtms
      in foldl TmApp (TmTApp tm (desugarTy rty)) cctms
desugarCase (TmTApp tm@(TmVar i) rty) tmtms =
  let cctms = desugarCCases tmtms
      in foldl TmApp (TmTApp tm (desugarTy rty)) cctms
desugarCase x tmtms =
  let cctms = desugarCCases tmtms
      in foldl TmApp x cctms

desugarCCases :: [(Term, Term)] -> [Term]
desugarCCases tmtms = map desugarCCase tmtms

desugarCCase :: (Term, Term) -> Term
desugarCCase (TmConstr c vs (TyCase ctys), tm) =
  let is = [i | (TmVar i) <- vs]
      is' = if null is then ["#unit"] else is
      tyis = snd $ (filter (\(c',_) -> if c == c' then True else False) ctys) !! 0
      tm' = desugarTm tm
      in foldr (\(i,ty) a -> TmAbs i (desugarTy ty) a) tm' (zip is' tyis)

desugarFTy :: Type -> F.Type
desugarFTy TyUnit = F.TyUnit
desugarFTy TyBool = F.TyBool
desugarFTy (TyVar i) = F.TyVar i
desugarFTy (TyAbs ty1 ty2) = F.TyAbs ty1' ty2'
  where ty1' = desugarFTy ty1
        ty2' = desugarFTy ty2
desugarFTy (TyTAbs i ty) = F.TyTAbs i ty'
  where ty' = desugarFTy ty
desugarFTy ty@(TyCase ctys) = desugarFTy ty'
  where ty' = desugarTy ty

desugarFTm :: Term -> F.Term
desugarFTm TmUnit = F.TmUnit
desugarFTm TmTrue = F.TmTrue
desugarFTm TmFalse = F.TmFalse
desugarFTm (TmVar i) = F.TmVar i
desugarFTm (TmAbs i ty tm) = F.TmAbs i ty' tm'
  where ty' = desugarFTy ty
        tm' = desugarFTm tm
desugarFTm (TmApp tm1 tm2) = F.TmApp tm1' tm2'
  where tm1' = desugarFTm tm1
        tm2' = desugarFTm tm2
desugarFTm (TmTAbs i tm) = F.TmTAbs i tm'
  where tm' = desugarFTm tm
desugarFTm (TmTApp tm ty) = F.TmTApp tm' ty'
  where tm' = desugarFTm tm
        ty' = desugarFTy ty
desugarFTm (TmLet itms tm) = desugarFTm tm'
  where tm' = desugarTm (TmLet itms tm)
desugarFTm (TmConstr c tms ty) = desugarFTm tm'
  where tm' = desugarTm (TmConstr c tms ty)
desugarFTm (TmCase tm tmtms) = desugarFTm tm'
  where tm' = desugarTm (TmCase tm tmtms)


sugarTy :: F.Type -> Type
sugarTy F.TyUnit = TyUnit
sugarTy F.TyBool = TyBool
sugarTy (F.TyVar i) = TyVar i
sugarTy (F.TyAbs ty1 ty2) = TyAbs ty1' ty2'
  where ty1' = sugarTy ty1
        ty2' = sugarTy ty2
sugarTy (F.TyTAbs i ty) = TyTAbs i ty'
  where ty' = sugarTy ty

sugarTm :: F.Term -> Term
sugarTm F.TmUnit = TmUnit
sugarTm F.TmTrue = TmTrue
sugarTm F.TmFalse = TmFalse
sugarTm (F.TmVar i) = TmVar i
sugarTm (F.TmAbs i ty tm) = TmAbs i (sugarTy ty) (sugarTm tm)
sugarTm (F.TmApp tm1 tm2) = TmApp (sugarTm tm1) (sugarTm tm2)
sugarTm (F.TmTAbs i tm) = TmTAbs i (sugarTm tm)
sugarTm (F.TmTApp tm ty) = TmTApp (sugarTm tm) (sugarTy ty)


{- =============================== Learning  =================================-}

learn :: Term -> Term
learn tm = learn' tm ([],(Map.empty,freshTmVars))

learn' :: Term -> (Context,Env) -> Term
learn' TmUnit cenv = TmUnit
learn' TmTrue cenv = TmTrue
learn' TmFalse cenv = TmFalse
learn' (TmVar i) cenv = (TmVar i)
learn' (TmAbs i ty tm) cenv = TmAbs i ty (learn' tm cenv)
learn' (TmApp tm1@(TmAbs i ty tm) tm2) (ctx,(m,fvs)) = TmApp tm1' tm2'
  where cenv = ((TmBind i ty):ctx, (Map.insert i (Left tm2) m, fvs))
        tm1' = learn' tm1 cenv
        tm2' = learn' tm2 (ctx,(m,fvs))
learn' (TmApp tm1 tm2) cenv = TmApp (learn' tm1 cenv) (learn' tm2 cenv)
learn' (TmTAbs i tm) cenv = TmTAbs i (learn' tm cenv)
learn' (TmTApp tm@(TmTAbs i tm') ty) (ctx,(m,fvs)) = TmTApp tm' ty
  where cenv = ((TyBind i):ctx, (Map.insert i (Right ty) m, fvs))
        tm' = learn' tm cenv
learn' (TmTApp tm ty) cenv = TmTApp (learn' tm cenv) ty
learn' (TmLet itms tm) cenv = TmLet itms' tm'
  where fitms = learnItms itms cenv
        itms' = fst fitms
        tm' = learn' tm (snd fitms)
learn' (TmCase tm tmtms) cenv = TmCase tm' tmtms'
  where tm' = learn' tm cenv
        tmtms' = map (\(tm1,tm2) -> (learn' tm1 cenv, learn' tm2 cenv)) tmtms
learn' (TmConstr c tms ty) cenv = TmConstr c tms' ty
  where tms' = map (\x -> learn' x cenv) tms
learn' (TmLSpec exs ty) (ctx,(m,fvs)) = sugarTm (L.learnTerm ty' exs' ctx' env')
  where ty' = desugarFTy ty
        exs' = desugarFExs exs
        ctx' = desugarFCtx ctx
        env' = (Map.map desugarFEnv m, fvs)

desugarFEnv :: Either Term Type -> Either F.Term F.Type
desugarFEnv (Left tm) = Left $ desugarFTm tm
desugarFEnv (Right ty) = Right $ desugarFTy ty

learnItms :: [(Id,Term)] -> (Context,Env) -> ([(Id,Term)], (Context,Env))
learnItms [] cenv = ([], cenv)
learnItms ((i,tm):itms) cenv@(ctx,(m,fvs)) = ((i,tm'):itms', cenv')
  where itms' = fst (learnItms itms cenv')
        tm' = learn' tm cenv
        (Right ty) = typeCheck tm ctx
        ctx' = (TmBind i ty):ctx
        m' = Map.insert i (Left tm') m
        cenv' = (ctx',(m',fvs))
