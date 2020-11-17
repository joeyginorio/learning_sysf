{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Learning.hs
==============================================================================
Defines synthesis rules from types and examples. Follows the specification in
Chapter 3 and 4 of "Learning in System F".
-}
module Learning where

import F
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.MemoTrie

import GHC.Generics (Generic)

{-========================= Generators from Type =============================-}

-- Calculates size of a type's AST
sizeType :: Type -> Int
sizeType (TyUnit) = 1
sizeType (TyBool) = 1
sizeType (TyVar _) = 1
sizeType (TyAbs typ1 typ2) = 1 + (sizeType typ1) + (sizeType typ2)
sizeType (TyTAbs _ typ) = 1 + (sizeType typ)

-- Generates all type variables
genTyVars :: Context -> [Type]
genTyVars ctx = [TyVar i | (TyBind i) <- ctx]

-- Generates all type abstractions to some AST depth n
genTyAbs :: Context -> Int -> [Type]
genTyAbs ctx n =
  let cartProd xs ys = [(x,y) | x <- xs, y <- ys]
      szs = [(n1, n - 1 - n1) | n1 <- [1..(n-1)]]
      typs = [(genITypes ctx (fst sz), genITypes ctx (snd sz)) | sz <- szs]
      abss = foldr (++) [] [cartProd ty1s ty2s | (ty1s, ty2s) <- typs]
      in [TyAbs typ1 typ2 | (typ1, typ2) <- abss]

-- Generates all universal type abstractions to some AST depth n
genTyTAbs :: Context -> Int -> [Type]
genTyTAbs _ 0 = []
genTyTAbs ctx n =
  let tyvs = [i | (TyBind i) <- ctx]
      typs = genITypes ctx (n-1)
      in [TyTAbs i typ | i <- tyvs,
                         typ <- (genITypes
                                (filter (\x -> x /= TyBind i) ctx)
                                (n-1))]

-- Generates all term variables at type
genTmVars :: Type -> Context -> [Term]
genTmVars typ ctx = [TmVar i | (TmBind i typ') <- ctx,
                                betaEqualTy typ' typ freshTyVars]

extractFTo2 :: Type -> Type -> Context -> [Id] -> Int -> Set.Set Type
extractFTo2 ty t@(TyAbs _ ty2) ctx is n
  | ty == ty2 = Set.singleton t
  | otherwise = extractFTo2 ty ty2 ctx is n
extractFTo2 ty t@(TyTAbs i ty') ctx is n
  | i `elem` is = Set.empty
  | otherwise = foldr Set.union Set.empty ts
  where ts' = [subType i fty ty' freshTyVars | (TmBind _ fty) <- ctx]
        ts  = [extractFTo2 ty t' ctx (i:is) n | t' <- ts']
extractFTo2 ty t ctx is n = Set.empty

extractTFTo2 :: Type -> Type -> Context -> Int -> Set.Set (Type, Type)
extractTFTo2 ty1 t@(TyTAbs i ty2) ctx n = Set.fromList ts
  where ts = [(t,fty)| (TmBind _ fty) <- ctx,
                       (subType i fty ty2 freshTyVars) == ty1]

extractFTo :: Type -> Type -> Context -> Set.Set Type
extractFTo ty t@(TyAbs _ ty2) ctx
  | ty == ty2 = Set.singleton t
  | otherwise = extractFTo ty ty2 ctx
extractFTo ty t@(TyTAbs i ty') ctx = Set.fromList ts
  where l1s = unfoldTypes ty
        l2s = replicate (length l1s) (unfoldType t [] freshTyVars)
        l1l2s = [(l11,l22) | (l11,l22) <- zip l1s l2s,
                             length l22 > length l11]
        l1s' = [reverse l11 | (l11,l22) <- l1l2s]
        l2s' = [fst . splitAt (length l11 + 1) . reverse $ l22 | (l11,l22) <- l1l2s]
        css = [getConstr2 l11' l22' | (l11',l22') <- zip l1s' l2s']
        fsss = [genFromConstr (reverse l22') cs ctx | (l22',Just cs) <- zip l2s' css]
        ts = concat [[foldr1 TyAbs fs | fs <- fss] | fss <- fsss]
extractFTo ty t ctx = Set.empty

-- get constraints for type variables in ty2s to turn into ty1s
getConstr2 :: [Type] -> [Type] -> Maybe (Map.Map Type Type)
getConstr2 (ty1:ty1s) (ty2@(TyVar i):ty2s)
  | ty1 /= ty2 = case (getConstr2 ty1s ty2s) of
                   Nothing -> Nothing
                   Just m -> case Map.lookup ty2 m of
                               Nothing -> Just $ Map.insert ty2 ty1 m
                               (Just x) -> if x /= ty1 then Nothing
                                           else getConstr2 ty1s ty2s
  | otherwise = getConstr2 ty1s ty2s
getConstr2 (ty1:ty1s) (ty2:ty2s)
  | ty1 /= ty2 = Nothing
  | otherwise  = getConstr2 ty1s ty2s
getConstr2 ty1s ty2s = Just $ Map.empty


-- get constraints for type variables in ty2s to turn into ty1s
getConstr :: [Type] -> [Type] -> Maybe [(Type,Type)]
getConstr (ty1:ty1s) (ty2@(TyVar i):ty2s)
  | ty1 /= ty2 = case (getConstr ty1s ty2s) of
                   Nothing -> Nothing
                   Just x -> Just $ (ty2,ty1):x
  | otherwise = getConstr ty1s ty2s
getConstr (ty1:ty1s) (ty2:ty2s)
  | ty1 /= ty2 = Nothing
  | otherwise  = getConstr ty1s ty2s
getConstr ty1s ty2s = Just []

genFromConstr :: [Type] -> Map.Map Type Type -> Context -> [[Type]]
genFromConstr tys@(ty@(TyVar i):tys') m ctx =
  case Map.lookup ty m of
    (Just x) -> [map f tys]
    Nothing  -> [nty:ttys | nty <- ntys]
  where f = (\x -> case Map.lookup x m of
                     Nothing -> x
                     (Just y) -> y)
        htys = head tys
        ttys = map f (tail tys)
        -- ntys = genTypes ctx n
        ntys = []
genFromConstr tys m ctx =
  [map f tys]
  where f = (\x -> case Map.lookup x m of
                       Nothing  -> x
                       (Just y) -> y)


checkConstr :: [(Type, Type)] -> Map.Map Type Type -> Bool
checkConstr ((ty1,ty2):tycs) m = case Map.lookup ty1 m of
  Nothing   -> checkConstr tycs (Map.insert ty1 ty2 m)
  Just ty2' -> if ty2 /= ty2'
                  then False
                  else checkConstr tycs m
checkConstr [] m = True

foldlN n f xs = (foldl1 f $ take n xs) : drop n xs

unfoldTypes :: Type -> [[Type]]
unfoldTypes t@(TyTAbs _ _) = [[t]]
unfoldTypes ty = l1:[unfoldType' ty [] n freshTyVars | n <- [0..(length l1 - 2)]]
  where l1 = unfoldType ty [] freshTyVars


unfoldType :: Type -> [Id] -> [Id] -> [Type]
unfoldType TyUnit is fvs = [TyUnit]
unfoldType TyBool is fvs = [TyBool]
unfoldType (TyVar x) is fvs = [TyVar x]
unfoldType (TyAbs ty1 ty2) is fvs = ty1 : unfoldType ty2 is fvs
unfoldType (TyTAbs i ty) is (x:xs)
  | i `elem` is = unfoldType (subType i (TyVar x) ty xs) (x:is) xs
  | otherwise   = unfoldType ty (i:is) (x:xs)

unfoldType' :: Type -> [Id] -> Int -> [Id] -> [Type]
unfoldType' TyUnit is n fvs = [TyUnit]
unfoldType' TyBool is n fvs = [TyBool]
unfoldType' (TyVar x) is n fvs = [TyVar x]
unfoldType' t@(TyAbs ty1 ty2) is n fvs
  | n > 0 = ty1 : unfoldType' ty2 is (n-1) fvs
  | otherwise = t:[]
unfoldType' t@(TyTAbs i ty) is n (x:xs) = [t]

-- short circuits if size isnt right
extractTFTo' :: Type -> Type -> Int -> Set.Set (Type,Type)
extractTFTo' ty1 ty2 n = case Set.toList (extractTFTo ty1 ty2) of
  [] -> Set.empty
  (tyty@(_,ty):_) -> if sizeType ty == n
                     then Set.singleton tyty
                     else Set.empty

-- be sure to short circuit immediately e.g. TF from X.Int->X to X.Bool->X
extractTFTo :: Type -> Type -> Set.Set (Type, Type)
extractTFTo (TyUnit) t@(TyTAbs i ty'@(TyVar x))
  | x == i        = Set.singleton (t,TyUnit)
  | otherwise     = Set.empty
extractTFTo (TyUnit) t@(TyTAbs i ty')
  | TyUnit == ty' = Set.singleton (t,TyUnit)
  | otherwise     = Set.empty
extractTFTo (TyBool) t@(TyTAbs i ty'@(TyVar x))
  | TyBool == ty' = Set.singleton (t,TyBool)
  | x == i        = Set.singleton (t,TyBool)
  | otherwise     = Set.empty
extractTFTo (TyBool) t@(TyTAbs i ty')
  | TyBool == ty' = Set.singleton (t,TyBool)
  | otherwise     = Set.empty
extractTFTo (TyVar x) t@(TyTAbs i ty'@(TyVar x'))
  | (TyVar x) == ty' = Set.singleton (t, TyVar x)
  | x' == i          = Set.singleton (t, TyVar x)
  | otherwise        = Set.empty
extractTFTo t1@(TyAbs _ _) t2@(TyTAbs i t2'@(TyAbs _ _)) =
  case (getTSub t1 t2' i Nothing) of
    Nothing -> Set.empty
    (Just ty) -> Set.singleton (t2, ty)
extractTFTo t1@(TyAbs _ _) t2@(TyTAbs i (TyVar x))
  | x == i    = Set.singleton (t2, t1)
  | otherwise = Set.empty
extractTFTo t1@(TyTAbs _ _) t2@(TyTAbs i t2'@(TyTAbs _ _)) =
  case (getTSub t1 t2' i Nothing) of
    Nothing -> Set.empty
    (Just ty) -> Set.singleton (t2, ty)
extractTFTo t1@(TyTAbs _ _) t2@(TyTAbs i (TyVar x))
  | x == i    = Set.singleton (t2, t1)
  | otherwise = Set.empty
extractTFTo ty t = Set.empty

-- extracts which type substitution would make the 2nd type into the first one
-- for some type variable specified by ID
-- initialized with Nothing for Maybe Type
getTSub :: Type -> Type -> Id -> Maybe Type -> Maybe Type
getTSub TyUnit ty i mty = case (ty,mty) of
  (TyUnit, mty')      -> mty
  (TyVar x, Nothing)  -> if x == i then Just TyUnit else Nothing
  (TyVar x, Just ty') -> if x == i && ty' == TyUnit then mty else Nothing
  otherwise           -> Nothing
getTSub TyBool ty i mty = case (ty,mty) of
  (TyBool, mty')      -> mty
  (TyVar x, Nothing)  -> if x == i then Just TyBool else Nothing
  (TyVar x, Just ty') -> if x == i && ty' == TyBool then mty else Nothing
  otherwise           -> Nothing
getTSub (TyVar y) ty i mty = case (ty,mty) of
  (TyVar x, Nothing)  -> if x == i then Just (TyVar y) else Nothing
  (TyVar x, Just ty') -> if x == i && ty' == (TyVar y) then mty else Nothing
  otherwise           -> Nothing
getTSub t@(TyAbs ty1 ty2) ty i mty = case (ty,mty) of
  ((TyAbs (TyVar x) ty2'), Nothing)  -> if x == i
                                        then getTSub ty2 ty2' i (Just ty1)
                                        else Nothing
  ((TyAbs (TyVar x) ty2'), Just ty') -> if x == i && ty' == ty1
                                        then getTSub ty2 ty2' i mty
                                        else Nothing
  (TyVar x, Nothing)                 -> if x == i then Just t else Nothing
  (TyVar x, Just ty')                -> if x == i && ty' == t then mty else Nothing
  otherwise                          -> Nothing
getTSub t@(TyTAbs x ty1) ty i mty = case (ty,mty) of
  (TyVar x, Nothing)  -> if x == i then Just t else Nothing
  (TyVar x, Just ty') -> if x == i && ty' == t then mty else Nothing
  otherwise           -> Nothing

-- Extracts the return type of a type
retType :: Type -> Type
retType (TyUnit) = TyUnit
retType (TyBool) = TyBool
retType (TyVar i) = TyVar i
retType (TyAbs typ1 typ2) = retType typ2
retType (TyTAbs i typ) = retType typ

cleanCtx :: Context -> Int -> Set.Set Type
cleanCtx [] n = Set.empty
cleanCtx ((TyBind _):bs) n = cleanCtx bs n
cleanCtx ((TmBind f typ):bs) n
  | sizeType typ < n = Set.insert typ (cleanCtx bs n)
  | otherwise        = cleanCtx bs n

cleanCtx2 :: Context -> Int -> Set.Set Type
cleanCtx2 [] n = Set.empty
cleanCtx2 ((TyBind _):bs) n = cleanCtx2 bs n
cleanCtx2 ((TmBind f typ):bs) n
  | sizeType typ == n = Set.insert typ (cleanCtx2 bs n)
  | otherwise         = cleanCtx2 bs n

-- Extracts all function types to a type given a context
extractFsTo :: Type -> Context -> Int -> [Type]
extractFsTo typ ctx n =
  let tys = Set.toList $ cleanCtx ctx n
      ftyps = List.foldl'
              (++)
              []
              [Set.toList $ extractFTo2 typ ty ctx [] n | ty <- tys]
      in ftyps

extractTFsTo :: Type -> Context -> Int -> [(Type,Type)]
extractTFsTo typ ctx n = Set.toList (foldr Set.union Set.empty
             [extractTFTo2 typ ftyp ctx n | ftyp@(TyTAbs _ _) <- ctx'])
  where ctx' = Set.toList $ cleanCtx2 ctx n

-- Generates all term applications at type to some AST depth n
genTmApps :: Type -> Context -> Int -> [Term]
genTmApps typ12 ctx n =
  let szs = [(n1, n - 1 - n1) | n1 <- [1..(n-1)]]
      fxs = [(x,y) | sz <- szs,
                     typ@(TyAbs typ11' _) <- extractFsTo typ12 ctx (fst sz),
                     x <- genETerms typ ctx (fst sz),
                     y <- genITerms typ11' ctx (snd sz)]
      in [TmApp f x | (f,x) <- fxs]

-- Generates all type applications at type to some AST depth n
genTmTApps :: Type -> Context -> Int -> [Term]
genTmTApps typ ctx n =
  let cartProd xs ys = [(x,y) | x <- xs, y <- ys]
      szs = [(n1, n - 1 - n1) | n1 <- [1..(n-1)]]
      fxs = [(genETerms ftyp ctx (fst sz), [x]) |
             sz <- szs,
             (ftyp, x) <- extractTFsTo typ ctx (snd sz)]
      tapps = foldr (++) [] [cartProd fs xs | (fs, xs) <- fxs]
      in [TmTApp f x | (f,x) <- tapps]

-- Generates all possible type applications at type (independent of context)
genTAppsType :: Type -> [(Type, Type)]
genTAppsType typ =
  let subtyps = Set.toList (getTypes typ)
      subtypsCount = [countType styp typ | styp <- subtyps]
      condss = [tail (sequence (replicate scount [False,True])) |
                scount <- subtypsCount]
      i = "$TApp" ++ show (sizeType typ)
      fxs = [(TyTAbs i
              (mapCondType (\x -> if x == styp then (TyVar i) else x)
              styp
              cond
              typ), styp) | (styp,conds) <- (zip subtyps condss), cond <- conds]
      in (TyTAbs i typ, TyUnit):fxs

-- generates all posible type apps at base type, with applied type at size n
genTAppsTy :: Type -> Int -> [(Type,Type)]
genTAppsTy ty n = genTAppsTyp ty rtys n
  where rtys = filter (\x -> sizeType x == n) (Set.toList $ getTypes ty)

genTAppsTyp :: Type -> [Type] -> Int -> [(Type,Type)]
genTAppsTyp ty rtys n = concat [genTAppsTypeN ty rty n | rty <- rtys]

genTAppsTypeN :: Type -> Type -> Int -> [(Type,Type)]
genTAppsTypeN ty rty n = map (\x -> (TyTAbs ("$TApp" ++ show n) x, rty)) tys
  where tys = genTAppsType' ty rty n

genTAppsType' :: Type -> Type -> Int -> [Type]
genTAppsType' TyUnit rty n
  | TyUnit == rty = TyUnit:[TyVar ("$TApp" ++ show n)]
  | otherwise     = [TyUnit]
genTAppsType' TyBool rty n
  | TyBool == rty = TyBool:[TyVar ("$TApp" ++ show n)]
  | otherwise     = [TyBool]
genTAppsType' (TyVar i) rty n
  | (TyVar i) == rty = (TyVar i):[TyVar ("$TApp" ++ show n)]
  | otherwise        = [(TyVar i)]
genTAppsType' ty@(TyAbs ty1 ty2) rty n
  | ty == rty  = ty:[TyVar ("$TApp" ++ show n)]
  | ty1 == rty = [TyAbs ty1 ty2' | ty2' <- genTAppsType' ty2 rty n] ++
                 [TyAbs (TyVar ("$TApp" ++ show n)) ty2' | ty2' <- genTAppsType'
                                                                   ty2
                                                                   rty
                                                                   n]
  | otherwise  = [TyAbs ty1 ty2' | ty2' <- genTAppsType' ty2 rty n]
genTAppsType' ty@(TyTAbs i ty') rty n
  | ty == rty = ty:[TyVar ("$TApp" ++ show n)]
  | otherwise = [TyTAbs i ty'' | ty'' <- genTAppsType' ty' rty n]


-- Maps a function to each subtype in a type, conditional on some proposition
mapCondType :: (Type -> Type) -> Type -> [Bool] -> Type -> Type
mapCondType f styp [] typ = typ
mapCondType f styp (b:bs) (TyUnit)
  | styp == TyUnit && b = f TyUnit
  | otherwise           = TyUnit
mapCondType f styp (b:bs) (TyBool)
  | styp == TyBool && b = f TyBool
  | otherwise           = TyBool
mapCondType f styp (b:bs) typ@(TyVar i)
  | styp == typ && b = f typ
  | otherwise        = typ
mapCondType f styp (b:bs) typ@(TyAbs typ1 typ2)
  | styp == typ && b = f typ
  | styp == typ1     = (TyAbs (mapCondType f styp (b:bs) typ1)
                              (mapCondType f styp (bs) typ2))
  | styp == typ2     = (TyAbs (mapCondType f styp (bs) typ1)
                              (mapCondType f styp (b:bs) typ2))
  | otherwise        = (TyAbs (mapCondType f styp (fst splitConds) typ1)
                              (mapCondType f styp (snd splitConds) typ2))
                       where stypCount1 = countType styp typ1
                             splitConds = splitAt stypCount1 (b:bs)
mapCondType f styp (b:bs) typ@(TyTAbs i typ')
  | styp == typ && b = f typ
  | otherwise        = (TyTAbs i (mapCondType f styp (b:bs) typ'))

-- Extracts set of subtypes in a type
getTypes :: Type -> Set.Set Type
getTypes typ@(TyAbs typ1 typ2) = Set.insert typ (Set.union (getTypes typ1)
                                                           (getTypes typ2))
getTypes typ@(TyTAbs i typ') = Set.singleton typ
getTypes typ = Set.singleton typ

getTysCtx :: Context -> [Type]
getTysCtx = Set.toList .  getTysCtx'

getTysCtx' :: Context -> Set.Set Type
getTysCtx' [] = Set.empty
getTysCtx' ((TyBind x):bs) = Set.insert (TyVar x) (getTysCtx' bs)
getTysCtx' ((TmBind _ ty):bs) = Set.union (getTypes ty) (getTysCtx' bs)

-- Counts instances of subtype in type
countType :: Type -> Type -> Int
countType styp typ@(TyAbs typ1 typ2)
  | styp == typ = 1
  | otherwise   = countType styp typ1 + countType styp typ2
countType styp typ@(TyTAbs i typ')
  | styp == typ = 1
  | otherwise   = countType styp typ'
countType styp typ
  | styp == typ = 1
  | otherwise   = 0

-- Generates all elimination terms at type to some AST depth n
-- genETerms :: Type -> Context -> Int -> [Term]
-- genETerms = memo3 genETerms'

genETerms :: Type -> Context -> Int -> [Term]
genETerms  _ _ 0 = []
genETerms (TyUnit) ctx 1 = genTmVars TyUnit ctx
genETerms (TyUnit) ctx n = (genTmApps TyUnit ctx n) ++
                           (genTmTApps TyUnit ctx n)
genETerms (TyBool) ctx 1 = genTmVars TyBool ctx
genETerms (TyBool) ctx n = (genTmApps TyBool ctx n) ++
                           (genTmTApps TyBool ctx n)
genETerms (TyVar i) ctx 1 = genTmVars (TyVar i) ctx
genETerms (TyVar i) ctx n = (genTmApps (TyVar i) ctx n) ++
                            (genTmTApps (TyVar i) ctx n)
genETerms typ@(TyAbs _ _) ctx 1 = genTmVars typ ctx
genETerms typ@(TyAbs _ _) ctx n = (genTmApps typ ctx n) ++
                                  (genTmTApps typ ctx n)
genETerms typ@(TyTAbs _ _) ctx 1 = genTmVars typ ctx
genETerms typ@(TyTAbs _ _) ctx n = (genTmApps typ ctx n) ++
                                   (genTmTApps typ ctx n)


-- Generates all introduction terms at type to some AST depth n
-- genITerms :: Type -> Context -> Int -> [Term]
-- genITerms = memo3 genITerms'

genITerms :: Type -> Context -> Int -> [Term]
genITerms _ _ 0 = []
genITerms (TyUnit) ctx 1 = [TmUnit] ++ (genETerms TyUnit ctx 1)
genITerms (TyUnit) ctx n = genETerms TyUnit ctx n
genITerms (TyBool) ctx 1 = [TmTrue, TmFalse] ++ (genETerms TyBool ctx 1)
genITerms (TyBool) ctx n = genETerms TyBool ctx n
genITerms (TyVar i) ctx n = genETerms (TyVar i) ctx n
genITerms typ@(TyAbs typ11 typ12) ctx 1 = genETerms typ ctx 1
genITerms typ@(TyAbs typ11 typ12) ctx n =
  let i = "x" ++ (show n)
      sz = sizeType typ11
      tms = genITerms typ12 ((TmBind i typ11):ctx) (n-sz-1)
      in [TmAbs i typ11 tm | tm <- tms] ++ (genETerms typ ctx n)
genITerms typ@(TyTAbs _ _) ctx 1 = genETerms typ ctx 1
genITerms typ@(TyTAbs i typ') ctx n =
  let tms = genITerms typ' ((TyBind i):ctx) (n-1)
      in [TmTAbs i tm | tm <- tms] ++ (genETerms typ ctx n)

-- Generates all elimination types to some AST depth n
-- genETypes :: Context -> Int -> [Type]
-- genETypes = memo2 genETypes'

genETypes :: Context -> Int -> [Type]
genETypes _ 0 = []
genETypes ctx 1 = (genTyVars ctx)
genETypes ctx n = []

-- Generates all introduction types to some AST depth n
-- genITypes :: Context -> Int -> [Type]
-- genITypes = memo2 genITypes'

genITypes :: Context -> Int -> [Type]
genITypes _ 0 = []
genITypes ctx 1 = [TyUnit, TyBool] ++ genETypes ctx 1
genITypes ctx n = (genTyAbs ctx n) ++ (genTyTAbs ctx n) ++ (genETypes ctx n)

-- Generates all types to some AST depth n
genTypes :: Context -> Int -> [Type]
genTypes ctx n = foldr (++) [] [genITypes ctx n' | n' <- [0..n]]

-- Generates all terms to some AST depth n
genTerms :: Type -> Context -> Int -> [Term]
genTerms typ ctx n = foldr (++) [] [genITerms typ ctx n' | n' <- [0..n]]


{-=============================== Examples ===================================-}

-- Examples are lists of tuples
data Example = Out Term
             | InTm Term Example
             | InTy Type Example
             deriving (Eq)

-- Pretty printing examples as tuples
instance Show Example where
  show e =
    let show' (Out t) = show t
        show' (InTm t e) = show t ++ "," ++ show' e
        show' (InTy t e) = show t ++ "," ++ show' e
        in "<" ++ show' e ++ ">"

-- Example length
lenExample :: Example -> Int
lenExample (Out trm) = 1
lenExample (InTm trm ex) = 1 + lenExample ex
lenExample (InTy typ ex) = 1 + lenExample ex

-- Beta equality of terms
betaEqualTm :: Term -> Term -> [Id] -> Bool
betaEqualTm tm1 tm2 is = betaEqualTm' tm1' tm2' is
  where tm1' = eval tm1 (Map.empty, freshTmVars)
        tm2' = eval tm2 (Map.empty, freshTmVars)


betaEqualTm' :: Term -> Term -> [Id] -> Bool
betaEqualTm' trm1@(TmAbs x1 typ1 _) trm2@(TmAbs x2 typ2 _) (i:is)
  | typ1 /= typ2 = False
  | otherwise = let (TmAbs _ _ trm1') = replaceTmVar x1 i trm1
                    (TmAbs _ _ trm2') = replaceTmVar x2 i trm2
                    in betaEqualTm' trm1' trm2' is
betaEqualTm' (TmTAbs x1 trm1) (TmTAbs x2 trm2) fvs@(i:is) =
  let trm1' = subTypeTerm x1 (TyVar i) trm1 fvs
      trm2' = subTypeTerm x2 (TyVar i) trm2 fvs
      in betaEqualTm' trm1' trm2' is
betaEqualTm' trm1 trm2 _ = trm1 == trm2

-- Beta equality of types
betaEqualTy :: Type -> Type -> [Id] -> Bool
betaEqualTy ty1@(TyTAbs i1 typ1) ty2@(TyTAbs i2 typ2) (i:is) =
  let (TyTAbs _ typ1') = replaceTyVar i1 i ty1
      (TyTAbs _ typ2') = replaceTyVar i2 i ty2
      in betaEqualTy typ1' typ2' is
betaEqualTy typ1 typ2 _ = typ1 == typ2


{-================== Generators from Type & Examples =========================-}

-- "Bad" substitution, i.e. not capture-avoiding. Necessary to fill holes.
badSubTm :: Id -> Term -> Term -> Term
badSubTm x trm (TmUnit) = TmUnit
badSubTm x trm (TmTrue) = TmTrue
badSubTm x trm (TmFalse) = TmFalse
badSubTm x trm (TmVar i)
  | x == i    = trm
  | otherwise = TmVar i
badSubTm x trm (TmAbs i typ trm') = (TmAbs i typ rtrm)
  where rtrm = badSubTm x trm trm'
badSubTm x trm (TmApp trm1 trm2) = (TmApp trm1' trm2')
  where trm1' = badSubTm x trm trm1
        trm2' = badSubTm x trm trm2
badSubTm x trm (TmTAbs i trm') = (TmTAbs i rtrm)
  where rtrm = badSubTm x trm trm'
badSubTm x trm (TmTApp trm' typ) = (TmTApp rtrm typ)
  where rtrm = badSubTm x trm trm'

-- Extracts the lhs of an application
extractAbs :: Term -> Term
extractAbs (TmApp trm1@(TmAbs _ _ _) trm2) = trm1
extractAbs (TmApp trm1 trm2) = extractAbs trm1
extractAbs (TmTApp trm@(TmTAbs _ _) typ) = trm
extractAbs (TmTApp trm typ) = extractAbs trm

-- Extracts the variables a term would add to the context
extractCtx :: Term -> Context
extractCtx (TmAbs i typ trm) = [TmBind i typ] ++ (extractCtx trm)
extractCtx (TmApp trm1 trm2) = (extractCtx trm1) ++ (extractCtx trm2)
extractCtx (TmTAbs i trm) = [TyBind i] ++ (extractCtx trm)
extractCtx (TmTApp trm typ) = extractCtx trm
extractCtx trm = []

-- Checks that a list of terms is beta equivalent to list of outputs
checkEx :: [Term] -> [Term] -> Env -> Bool
checkEx trms outs env =
  let trmouts = zip trms outs
      fvs = freshTmVars
      in all (\(trm,out) -> betaEqualTm (eval trm env) (eval out env) fvs) trmouts

-- Learn terms from types and examples
lrnTerms :: Type -> [Example] -> Context -> Env -> [Term] -> Int -> [Term]
lrnTerms typ exs ctx env ltrms 0 = []
lrnTerms typ exs ctx env [] n =
  let holes = replicate (length exs) (TmVar "$HOLE")
      in lrnTerms typ exs ctx env holes n
lrnTerms typ exs@((Out _):_) ctx env ltrms n =
  let ctx' = (extractCtx (extractAbs (ltrms !! 0))) ++ ctx
      htrms = genITerms typ ctx' n
      ltrms' = [[badSubTm "$HOLE" htrm ltrm | ltrm <- ltrms] |
                                              htrm <- htrms]
      otrms = [trm | (Out trm) <- exs]
      in [extractAbs trm | t@(trm:trms) <- ltrms', checkEx t otrms env]
lrnTerms (TyAbs typ1 typ2) exs@((InTm _ _):_) ctx env ltrms n =
  let i = "x" ++ show n
      strm = TmAbs i typ1 (TmVar "$HOLE")
      ftrms = [subTerm "$HOLE" strm ltrm freshTmVars | ltrm <- ltrms]
      itrms = [trm | (InTm trm _) <- exs]
      ltrms' = zipWith TmApp ftrms itrms
      exs' = [ex | (InTm _ ex) <- exs]
      sztyp = sizeType typ1
      in lrnTerms typ2 exs' ctx env ltrms' (n-1-sztyp)
lrnTerms (TyTAbs i typ) exs@((InTy _ _):_) ctx env ltrms n =
  let strm = TmTAbs i (TmVar "$HOLE")
      ftrms = [subTerm "$HOLE" strm ltrm freshTmVars | ltrm <- ltrms]
      ityps = [typ | (InTy typ _) <- exs]
      ltrms' = zipWith TmTApp ftrms ityps
      exs' = [ex | (InTy _ ex) <- exs]
      in lrnTerms typ exs' ctx env ltrms' (n-1)

learnTerms :: Type -> [Example] -> Context -> Env -> Int -> [Term]
learnTerms typ exs ctx env n = foldr
                               (++)
                               []
                               [lrnTerms typ exs ctx env [] n' | n' <- [0..n]]


learnTerm :: Type -> [Example] -> Context -> Env -> Term
learnTerm typ exs ctx env = getTm [lrnTerms typ exs ctx env [] n | n <- [0..]]

-- In a list of lists of terms, extracts first term
getTm :: [[Term]] -> Term
getTm xxs@(x:xs) = case (head . take 1) xxs of
                     [] -> getTm xs
                     _  -> (head . take 1) x

data Foo = Foo1 Int
         | Foo2 Int
         deriving (Show, Generic)

instance HasTrie Foo where
  newtype (Foo :->: b) = FooTrie {unFooTrie :: Reg Foo :->: b}
  trie = trieGeneric $! FooTrie
  untrie = untrieGeneric $! unFooTrie
  enumerate = enumerateGeneric $! unFooTrie


foo :: Foo -> Foo
foo = memoFix foo'

foo' :: (Foo -> Foo) -> Foo -> Foo
foo' f (Foo1 0) = Foo1 0
foo' f (Foo2 0) = Foo2 0
foo' f (Foo1 1) = Foo1 1
foo' f (Foo2 1) = Foo2 1
foo' f (Foo1 n) = f (Foo1 (n-1)) `foop` f (Foo1 (n-2))
foo' f (Foo2 n) = f (Foo2 (n-1)) `foop` f (Foo2 (n-2))

foop :: Foo -> Foo -> Foo
foop (Foo1 n1) (Foo1 n2) = Foo1 (n1 + n2)
foop (Foo1 n1) (Foo2 n2) = Foo1 (n1 + n2)
foop (Foo2 n1) (Foo1 n2) = Foo2 (n1 + n2)
foop (Foo2 n1) (Foo2 n2) = Foo2 (n1 + n2)
