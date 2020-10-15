{-
Parser for System F+
====================

prog ::= decl
         prog

decl ::= tydecl
         tmdecl
       | ddecl
         tydecl
         tmdecl

tydecl ::= identifier :: ty

tmdecl ::= identifier params = tm

ddecl ::= data identifier = [constructor [ty]]

tm ::= lam id : ty . tm | tm tm | x | tt | ff | unit | (tm)
     | let [identifier = tm] in tm | constructor [tm] ty
     | case identifier of [constructor [identifiers] -> tm]
ty ::= Bool | Unit | ty -> ty | <(id : type) + (id : type)> | (type)

Example Program:

data Foo = Foo1 Unit
         | Foo2 Unit Unit

foo1 :: Bool -> Bool
foo x = (\y z. tt)tt

foo2 :: Foo -> Unit
foo2 (Foo1 x) = x
foo2 (Foo2 x y) = x

-}

import MParser
import FPlus
import Control.Monad
import qualified F as F
import qualified Data.Map as M

type Prog = [Decl]

type Decl = Either (TyDecl, TmDecl) ([DDecl], TyDecl, TmDecl)
type TyDecl = (String, Type)
type TmDecl = (String, [String], Term)
type DDecl = (String, Type)


{-================================ PARSE TYPES ===============================-}

tyUnit :: Parser Type
tyUnit = do symbol "Unit"
            return TyUnit

tyBool :: Parser Type
tyBool = do symbol "Bool"
            return TyBool

tyAbsOp :: Parser (Type -> Type -> Type)
tyAbsOp = do symbol "->"
             return TyAbs

tyAtom :: Parser Type
tyAtom = tyUnit <|> tyBool <|> tyTAbs <|> tyVar <|> tyCase <|> parens ty

tyVar :: Parser Type
tyVar = do c <- constructor
           return $ TyVar c

tyTAbs :: Parser Type
tyTAbs = do c <- constructor
            symbol "."
            t <- ty
            return $ TyTAbs c t

tyCase :: Parser Type
tyCase = do ctys <- sepby (parens (do c <- constructor
                                      symbol ":"
                                      tys <- sepby ty (symbol "*")
                                      return (c,tys))) (symbol "+")
            return $ TyCase ctys

ty :: Parser Type
ty = tyAtom `chainr1` tyAbsOp


{-================================ PARSE TERMS ===============================-}

keywords :: [String]
keywords = ["let", "in", "lam", "data", "as", "case", "of"]

tmUnit :: Parser Term
tmUnit = do symbol "unit"
            return TmUnit

tmTrue :: Parser Term
tmTrue = do symbol "tt"
            return TmTrue

tmFalse :: Parser Term
tmFalse = do symbol "ff"
             return TmFalse

tmVar :: Parser Term
tmVar = do  x <- identifier keywords
            return $ TmVar x

tmAbs :: [Type] -> Parser Term
tmAbs tys = do  symbol "lam"
                x <- identifier keywords
                symbol ":"
                a <- ty
                symbol "."
                t <- tm tys
                return $ TmAbs x a t

tmLet :: [Type] -> Parser Term
tmLet tys = do symbol "let"
               itms <- many (do  x <- identifier keywords
                                 symbol "="
                                 t <- tm tys
                                 line
                                 return (x,t))
               symbol "in"
               t <- tm tys
               return $ TmLet itms t

tmConstr :: [Type] -> Parser Term
tmConstr [] = do c <- constructor
                 tms <- liftM f (many (tmAtom [] <|> tm []))
                 symbol "as"
                 a <- ty
                 return $ TmConstr c tms a
                   where f = (\x -> if x == [] then [TmUnit] else x)
tmConstr tys = do c <- constructor
                  tms <- liftM f (many (tmAtom tys <|> tm tys))
                  let a = tyFromC c tys
                  return $ TmConstr c tms a
                    where f = (\x -> if x == [] then [TmUnit] else x)

tyFromC :: Constr -> [Type] -> Type
tyFromC c (ty:[]) = ty
tyFromC c (ty:tys) = if tyInC c ty then ty else tyFromC c tys

tyInC :: Constr -> Type -> Bool
tyInC c (TyCase ctys) = any (\(c',tys) -> c == c') ctys
tyInc _ _ = False

tmCase :: [Type] -> Parser Term
tmCase tys = do symbol "case"
                t <- tm tys
                symbol "of"
                symbol "\n" <|> (symbol "")
                tmtms <- many (do t1 <- tm tys
                                  symbol "->"
                                  t2 <- tm tys
                                  (symbol ";" <|> symbol "\n" <|> symbol "")
                                  return (t1,t2))
                return $ TmCase t tmtms

tmAtom :: [Type] -> Parser Term
tmAtom tys = tmCase tys <|> tmLet tys <|> tmAbs tys <|> tmUnit <|> tmTrue <|>
             tmFalse <|> tmConstr tys <|> tmVar <|> parens (tm tys)

tmAppOp :: Parser (Term -> Term -> Term)
tmAppOp = return TmApp

tm :: [Type] -> Parser Term
tm tys = do (tmAtom tys) `chainl1` tmAppOp


{-================================ PARSE DECLS ===============================-}
tyDecl :: Parser TyDecl
tyDecl = do allSpace
            f <- identifier keywords
            symbol "::"
            a <- ty
            return (f,a)

tmDecl :: [Type] -> Parser TmDecl
tmDecl tys = do allSpace
                f <- identifier keywords
                ps <- many (identifier keywords)
                symbol "="
                t <- tm tys
                return (f,ps,t)

dDecl :: Parser DDecl
dDecl = do symbol "data"
           d <- constructor
           symbol "="
           c1 <- constructor
           tys1 <- liftM (\x -> if x == [] then [TyUnit] else x) (many ty)
           ctys <- many (do symbol "\n" <|> (symbol "")
                            symbol "|"
                            c <- constructor
                            tys <- many ty
                            return (c,tys))
           return (d, (TyCase ((c1,tys1):ctys)))


tytmDecl :: [Type] -> Parser Decl
tytmDecl tys = do tyd <- tyDecl
                  tmd <- tmDecl tys
                  return $ Left (tyd,tmd)

dtytmDecl :: [Type] -> Parser Decl
dtytmDecl tys = do dds <- many (do d <- dDecl
                                   many line
                                   return d)
                   tyd <- tyDecl
                   tmd <- tmDecl (((snd . unzip) dds) ++ tys)
                   return $ Right (dds ,tyd,tmd)

decl :: [Type] -> Parser Decl
decl tys = tytmDecl tys <|> dtytmDecl tys

{-================================ PARSE PROGS ===============================-}
prog :: Parser Prog
prog = prog' [] [] 20

prog' :: Prog -> [Type] -> Int -> Parser Prog
prog' p tys 0 = return p
prog' p tys n = (do (d,tys') <- decl' tys
                    prog' (p++[d]) (tys++tys') (n-1)) <|> prog' p tys (n-1)

decl' :: [Type] -> Parser (Decl, [Type])
decl' tys = do d <- decl tys
               return (d, tys ++ tysFromDecl d)

tysFromDecl :: Decl -> [Type]
tysFromDecl (Left _) = []
tysFromDecl (Right (dds,_,_)) = (snd . unzip) dds

parseFile :: String -> Parser a -> IO a
parseFile f p = fmap (fst . head . parse p) (readFile f)

-- Similar to parseFile, but includes info about parse for debugging.
parseFile' :: String -> Parser a -> IO [(a,String)]
parseFile' f p = fmap (parse p) (readFile f)


{-============================== DESUGAR PROGS ===============================-}
desugar :: Prog -> F.Term
desugar p = desugarTm . desugarProg' $ p
  where desugarProg' = (\p -> eval (desugarProg p) (M.empty, freshTmVars))

desugarProg :: Prog -> Term
desugarProg (d:[]) = desugarDecl d
desugarProg dds@(d@(Left ((f,_),_)):ds) = TmLet [(f,tm1)] tm2
  where tm1 = desugarDecl d
        tm2 = foldr (\(i,t) a -> subTypeTerm i t a fvs) dds' ddecls
        dds' = desugarDecl $ last dds
        fvs = freshTmVars
        ddecls = getDDecl dds
desugarProg dds@(d@(Right (_,(f,_),_)):ds) = TmLet [(f,tm1)] tm2
  where tm1 = desugarDecl d
        tm2 = foldr (\(i,t) a -> subTypeTerm i t a fvs) dds' ddecls
        dds' = desugarDecl $ last dds
        fvs = freshTmVars
        ddecls = getDDecl dds

getDDecl :: Prog -> [DDecl]
getDDecl [] = []
getDDecl ((Right (d,_,_)):ds) = d ++ getDDecl ds
getDDecl ((Left d):ds) = getDDecl ds

desugarDecl :: Decl -> Term
desugarDecl (Left ((_,_),(_,[],tm))) = tm
desugarDecl (Left ((_,(TyAbs ty1 ty2)),(f,(a:as),tm))) =
  TmAbs a ty1 $ desugarDecl (Left ((f,ty2),(f,as,tm)))
desugarDecl (Right ([],tyd,tmd)) = desugarDecl $ Left (tyd, tmd)
desugarDecl (Right (((f,ty@(TyCase ctys)):ds),tyd,tmd)) =
  subTypeTerm f ty tm freshTyVars
  where tm = (desugarDecl (Right (ds,tyd,tmd)))

buildConstr :: (Constr, [Type]) -> Type -> (Id, Term)
buildConstr (c,tys) ty = (c, foldr f (TmConstr c tms ty) tmtys)
  where tms = [TmVar i | i <- vs]
        vs = ["c" ++ show i | i <- [0..(length tys - 1)]]
        f = (\((TmVar i),ty) a -> TmAbs i ty a)
        tmtys = zip tms tys

buildConstrs :: [(Constr, [Type])] -> Type -> [(Id,Term)]
buildConstrs ctys ty = map (\x -> buildConstr x ty) ctys

desugarTy :: Type -> F.Type
desugarTy TyUnit = F.TyUnit
desugarTy TyBool = F.TyBool
desugarTy (TyVar i) = F.TyVar i
desugarTy (TyAbs ty1 ty2) = F.TyAbs ty1' ty2'
  where ty1' = desugarTy ty1
        ty2' = desugarTy ty2
desugarTy (TyTAbs i ty) = F.TyTAbs i ty'
  where ty' = desugarTy ty
desugarTy (TyCase ctys) = desugarTyCase ctys

desugarTyCase :: [(Constr, [Type])] -> F.Type
desugarTyCase ctys = F.TyTAbs "#R" ty1
  where ty1 = foldr (\x a -> F.TyAbs x a) (F.TyVar "#R") ty1'
        ty1' = desugarConstr ((snd . unzip) ctys)
        desugarConstr = map (foldr (\x a -> F.TyAbs (desugarTy x) a) (F.TyVar "#R"))

desugarTm :: Term -> F.Term
desugarTm TmUnit = F.TmUnit
desugarTm TmTrue = F.TmTrue
desugarTm TmFalse = F.TmFalse
desugarTm (TmVar i) = F.TmVar i
desugarTm (TmAbs i ty tm) = F.TmAbs i ty' tm'
  where ty' = desugarTy ty
        tm' = desugarTm tm
desugarTm (TmApp tm1 tm2) = F.TmApp tm1' tm2'
  where tm1' = desugarTm tm1
        tm2' = desugarTm tm2
desugarTm (TmTAbs i tm) = F.TmTAbs i tm'
  where tm' = desugarTm tm
desugarTm (TmTApp tm ty) = F.TmTApp tm' ty'
  where tm' = desugarTm tm
        ty' = desugarTy ty
desugarTm (TmLet itms tm) = foldr f tm' itms
  where tm' = desugarTm tm
        right = (\(Right x) -> x)
        typeCheck' = (\t -> desugarTy (right (typeCheck t [])))
        f = (\(i,t) a -> F.TmApp (F.TmAbs i (typeCheck' t) a) (desugarTm t))
desugarTm (TmConstr c tms ty) = desugarConstr c tms ty
desugarTm (TmCase tm tmtms) = desugarCase tm tmtms


desugarConstr :: Constr -> [Term] -> Type -> F.Term
desugarConstr c tms (TyCase ctys) = foldl (\a t -> F.TmApp a (desugarTm t)) tm tms
  where tyas = snd $ (filter (\(c',_) -> if c == c' then True else False) ctys) !! 0
        as = ["a" ++ show i | i <- [0..(length tyas - 1)]]
        tycs = getTycs ctys
        cs = ["c" ++ show i | i <- [0..(length tycs - 1)]]
        tm = foldr (\(i,t) a -> F.TmAbs i (desugarTy t) a) tm' (zip as tyas)
        tm' = F.TmTAbs "#R" tm''
        tm'' = foldr (\(i,t) a -> F.TmAbs i (desugarTy t) a) tm''' (zip cs tycs)
        ci = getCi c ctys
        tm''' = foldl1 F.TmApp ((F.TmVar (cs !! ci)):(map F.TmVar as))

getTycs :: [(Constr,[Type])] -> [Type]
getTycs [] = []
getTycs ((c,tys):ctys) = ty : (getTycs ctys)
  where ty = foldr (\x a -> TyAbs x a) (TyVar "#R") tys

getCi :: Constr -> [(Constr, [Type])] -> Int
getCi c [] = 0
getCi c ((c',tys):ctys) = if c == c' then 0 else 1 + getCi c ctys

desugarCase :: Term -> [(Term,Term)] -> F.Term
desugarCase dtm tmtms@((TmConstr _ _ ty@(TyCase ctys),rtm):tmtms') =
  let dty = desugarTy ty
      dtm' = desugarTm dtm
      tycs = getTycs ctys
      cs = ["c" ++ show i | i <- [0..(length tycs - 1)]]
      tm = F.TmAbs "#D" dty tm'
      tm' = F.TmTAbs "#R" tm''
      tm'' = foldr (\(i,t) a -> F.TmAbs i (desugarTy t) a) tm'''' (zip cs tycs)
      tm''' = (F.TmTApp (F.TmVar "#D") (F.TyVar "#R"))
      tm'''' = foldl F.TmApp tm''' (map F.TmVar cs)
      right = (\(Right x) -> x)
      ctx = [TmBind i ty | (TmVar i) <- [dtm]]
      rty = desugarTy $ right (typeCheck (TmCase dtm tmtms) ctx)
      cctms = desugarCCases tmtms
      in foldl F.TmApp (F.TmTApp (F.TmApp tm dtm') rty) cctms

desugarCCases :: [(Term, Term)] -> [F.Term]
desugarCCases tmtms = map desugarCCase tmtms

desugarCCase :: (Term, Term) -> F.Term
desugarCCase (TmConstr c vs (TyCase ctys), tm) =
  let is = [i | (TmVar i) <- vs]
      tyis = snd $ (filter (\(c',_) -> if c == c' then True else False) ctys) !! 0
      tm' = desugarTm tm
      in foldr (\(i,ty) a -> F.TmAbs i (desugarTy ty) a) tm' (zip is tyis)
