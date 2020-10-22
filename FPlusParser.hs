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
tmVar = do  x <- identifier keywords <|> constructor
            return $ TmVar x

tmAbs :: [Type] -> Parser Term
tmAbs tys = do  symbol "lam"
                x <- identifier keywords
                symbol ":"
                a <- ty
                symbol "."
                t <- tm tys
                return $ TmAbs x a t

tmTAbs :: [Type] -> Parser Term
tmTAbs tys = do symbol "forall"
                x <- constructor
                symbol "."
                t <- tm tys
                return $ TmTAbs x t

tmTApp :: [Type] -> Parser Term
tmTApp tys = do t <- tm tys
                symbol "["
                a <- ty
                symbol "]"
                return $ TmTApp t a

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
                 return $ TmTAbs "#R" (TmConstr c tms a)
                   where f = (\x -> if x == [] then [TmUnit] else x)
tmConstr tys = do c <- constructor
                  tms <- liftM f (many (tmAtom tys <|> tm tys))
                  let a = tyFromC c tys
                  return $ TmTAbs "#R" (TmConstr c tms a)
                    where f = (\x -> if x == [] then [TmUnit] else x)

tyFromC :: Constr -> [Type] -> Type
tyFromC c ((TyTAbs _ ty):[]) = ty
tyFromC c ((TyTAbs _ ty):tys) = if tyInC c ty then ty else tyFromC c tys

tyInC :: Constr -> Type -> Bool
tyInC c (TyCase ctys) = any (\(c',tys) -> c == c') ctys
tyInC _ _ = False

tmCase :: [Type] -> Parser Term
tmCase tys = do symbol "case"
                t <- tm tys
                let rty = tyFromTmTApp t
                let e = (M.empty,freshTmVars)
                symbol "of"
                symbol "\n" <|> (symbol "")
                tmtms <- many (do t1 <- tmConstr tys
                                  symbol "->"
                                  t2 <- tm tys
                                  (symbol ";" <|> symbol "\n" <|> symbol "")
                                  return (eval (TmTApp t1 rty) e,t2))
                return $ TmCase t tmtms

tyFromTmTApp :: Term -> Type
tyFromTmTApp (TmTApp _ ty) = ty

tmAtom :: [Type] -> Parser Term
tmAtom tys = tmCase tys <|> tmLet tys <|> tmAbs tys <|> tmTAbs tys <|>
             tmUnit <|> tmTrue <|> tmFalse <|> tmVar <|> parens (tm tys)

tmAppOp :: Parser (Term -> Term -> Term)
tmAppOp = return TmApp

tmTAppOp :: Parser (Term -> Type -> Term)
tmTAppOp = return TmTApp

tm :: [Type] -> Parser Term
tm tys =  ((chainl1' (tmAtom tys) ty' (tmTAppOp)) <|> (tmAtom tys))
           `chainl1` (tmAppOp)

ty' :: Parser Type
ty' = do symbol "["
         t <- ty
         symbol "]"
         return t

chainl1' :: Parser a -> Parser b -> Parser (a -> b -> a) -> Parser a
chainl1' p1 p2 op = do a <- p1
                       rest a
                         where rest a = do f <- op
                                           b <- p2
                                           rest (f a b)
                                        <|> return a


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
           let f = (\x -> case x of
                           (TyVar i) -> if d == i then (TyVar "#R") else x
                           otherwise -> x)
           let tys1' = map f tys1
           ctys <- many (do symbol "\n" <|> (symbol "")
                            symbol "|"
                            c <- constructor
                            tys <- liftM
                                    (\x -> if x == [] then [TyUnit] else x)
                                    (many ty)
                            let tys' = map f tys
                            return (c,tys'))
           return (d, TyTAbs "#R" (TyCase ((c1,tys1'):ctys)))


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
-- desugar :: Prog -> F.Term
-- desugar p = (desugarTm . desugarProg) p

-- desugarProg :: Prog -> Term
-- desugarProg (d:[]) = desugarDecl d
-- desugarProg dds@(d@(Left ((f,_),_)):ds) = TmLet [(f,tm1)] tm2
--   where tm1 = desugarDecl d
--         tm2 = foldr (\(i,t) a -> subTypeTerm i t a fvs) dds' ddecls
--         dds' = desugarDecl $ last dds
--         fvs = freshTmVars
--         ddecls = getDDecl dds
-- desugarProg dds@(d@(Right (_,(f,_),_)):ds) = TmLet [(f,tm1)] tm2
--   where tm1 = desugarDecl d
--         tm2 = foldr (\(i,t) a -> subTypeTerm i t a fvs) dds' ddecls
--         dds' = desugarDecl $ last dds
--         fvs = freshTmVars
--         ddecls = getDDecl dds

desugarProg :: Prog -> Term
desugarProg ds = foldr (\(i,t) a -> subTypeTerm i t a fvs) p dds
  where dds = getDDecl ds
        fvs = freshTmVars
        p = TmLet itms (TmVar f)
        itms = concat $ map desugarDecl ds
        (f,_) = last itms

getDDecl :: Prog -> [DDecl]
getDDecl [] = []
getDDecl ((Right (d,_,_)):ds) = d ++ getDDecl ds
getDDecl ((Left d):ds) = getDDecl ds

desugarDecl :: Decl -> [(Id,Term)]
desugarDecl (Left ((_,ty),(f,as,tm))) = [(f,tm')]
  where tm' = tmFromTyTmDecl ty as tm
desugarDecl (Right (ds,tyd,tmd)) = itmd ++ itmtytm
  where itmtytm = desugarDecl $ Left (tyd,tmd)
        itmd = concat $ map tmFromDDecl ds

tmFromDDecl :: DDecl -> [(Id,Term)]
tmFromDDecl (f,(TyTAbs "#R" ty@(TyCase ctys))) = buildConstrs ctys ty


tmFromTyTmDecl :: Type -> [Id] -> Term -> Term
tmFromTyTmDecl (TyAbs ty1 ty2) (a:as) tm = TmAbs a ty1 (tmFromTyTmDecl ty2 as tm)
tmFromTyTmDecl _ _ tm = tm

buildConstr :: (Constr, [Type]) -> Type -> (Id, Term)
buildConstr (c,tys) ty = if tmtys == [(TmVar "a0",TyUnit)]
                         then (c, TmApp (foldr f
                                         (TmTAbs "#R" $ TmConstr c tms ty)
                                         tmtys)
                                         TmUnit)
                         else (c, foldr f (TmTAbs "#R" $ TmConstr c tms ty) tmtys)
  where tys' = map (\x -> if x == (TyVar "#R")
                          then TyTAbs "#R" ty
                          else x) tys
        tms = [TmVar i | i <- vs]
        vs = ["a" ++ show i | i <- [0..(length tys - 1)]]
        f = (\((TmVar i),ty) a -> TmAbs i ty a)
        tmtys = zip tms tys'

buildConstrs :: [(Constr, [Type])] -> Type -> [(Id,Term)]
buildConstrs ctys ty = map (\x -> buildConstr x ty) ctys

desugarFTy :: Type -> F.Type
desugarFTy TyUnit = F.TyUnit
desugarFTy TyBool = F.TyBool
desugarFTy (TyVar i) = F.TyVar i
desugarFTy (TyAbs ty1 ty2) = F.TyAbs ty1' ty2'
  where ty1' = desugarFTy ty1
        ty2' = desugarFTy ty2
desugarFTy (TyTAbs i ty) = F.TyTAbs i ty'
  where ty' = desugarFTy ty
desugarFTy (TyCase ctys) = desugarFTyCase ctys

desugarFTyCase :: [(Constr, [Type])] -> F.Type
desugarFTyCase ctys = F.TyTAbs "#R" ty1
  where ty1 = foldr (\x a -> F.TyAbs x a) (F.TyVar "#R") ty1'
        ty1' = desugarConstr ((snd . unzip) ctys)
        desugarConstr = map (foldr (\x a -> F.TyAbs (desugarFTy x) a) (F.TyVar "#R"))

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
desugarFTm (TmLet itms tm) = foldr f tm' itms
  where tm' = desugarFTm tm
        right = (\(Right x) -> x)
        typeCheck' = (\t -> desugarFTy (right (typeCheck t [])))
        f = (\(i,t) a -> F.TmApp (F.TmAbs i (typeCheck' t) a) (desugarFTm t))
desugarFTm (TmConstr c tms ty) = desugarFConstr c tms ty
desugarFTm (TmCase tm tmtms) = desugarFCase tm tmtms


desugarFConstr :: Constr -> [Term] -> Type -> F.Term
desugarFConstr c tms (TyCase ctys) = foldl (\a t -> F.TmApp a (desugarFTm t)) tm tms
  where tyas = snd $ (filter (\(c',_) -> if c == c' then True else False) ctys) !! 0
        as = ["a" ++ show i | i <- [0..(length tyas - 1)]]
        tycs = getTycs ctys
        cs = ["c" ++ show i | i <- [0..(length tycs - 1)]]
        tm = foldr (\(i,t) a -> F.TmAbs i (desugarFTy t) a) tm' (zip as tyas)
        tm' = F.TmTAbs "#R" tm''
        tm'' = foldr (\(i,t) a -> F.TmAbs i (desugarFTy t) a) tm''' (zip cs tycs)
        ci = getCi c ctys
        tm''' = foldl1 F.TmApp ((F.TmVar (cs !! ci)):(map F.TmVar as))

desugarFCase :: Term -> [(Term,Term)] -> F.Term
desugarFCase dtm tmtms@((TmConstr _ _ ty@(TyCase ctys),rtm):tmtms') =
  let dty = desugarFTy ty
      dtm' = desugarFTm dtm
      tycs = getTycs ctys
      cs = ["c" ++ show i | i <- [0..(length tycs - 1)]]
      tm = F.TmAbs "#D" dty tm'
      tm' = F.TmTAbs "#R" tm''
      tm'' = foldr (\(i,t) a -> F.TmAbs i (desugarFTy t) a) tm'''' (zip cs tycs)
      tm''' = (F.TmTApp (F.TmVar "#D") (F.TyVar "#R"))
      tm'''' = foldl F.TmApp tm''' (map F.TmVar cs)
      right = (\(Right x) -> x)
      ctx = [TmBind i ty | (TmVar i) <- [dtm]]
      rty = desugarFTy $ right (typeCheck (TmCase dtm tmtms) ctx)
      cctms = desugarFCCases tmtms
      in foldl F.TmApp (F.TmTApp (F.TmApp tm dtm') rty) cctms

desugarFCCases :: [(Term, Term)] -> [F.Term]
desugarFCCases tmtms = map desugarFCCase tmtms

desugarFCCase :: (Term, Term) -> F.Term
desugarFCCase (TmConstr c vs (TyCase ctys), tm) =
  let is = [i | (TmVar i) <- vs]
      is' = if null is then ["#unit"] else is
      tyis = snd $ (filter (\(c',_) -> if c == c' then True else False) ctys) !! 0
      tm' = desugarFTm tm
      in foldr (\(i,ty) a -> F.TmAbs i (desugarFTy ty) a) tm' (zip is' tyis)


fix f = x where x = f x

foo :: Int -> Bool
foo 0 = False
foo n = foo (n-1)

foo' :: (Int -> Bool) -> Int -> Bool
foo' f 0 = False
foo' f n = f (n-1)
