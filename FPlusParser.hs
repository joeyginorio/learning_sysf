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

module FPlusParser where

import MParser
import FPlus
import Control.Monad
import qualified F as F
import qualified Data.Map as M

type Prog = [Decl]

type Decl = Either (TyDecl, TmDecl) ([DDecl], TyDecl, TmDecl)
type TyDecl = (String, Type)
type TmDecl = (String, [String], Either Term [Example])
type DDecl = (String, Type)


{-================================ PARSE TYPES ===============================-}

tyUnit :: Parser Type
tyUnit = do symbol "Unit"
            return TyUnit

tyBool :: Parser Type
tyBool = do symbol "Boolean"
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
                                  return (eval' (TmTApp t1 rty) e,t2))
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


{-============================= PARSE EXAMPLES ===============================-}

tmLSpec :: [Type] -> Parser Term
tmLSpec tys = do symbol "("
                 xs <- exs tys
                 symbol "::"
                 t <- ty
                 symbol ")"
                 return $ TmLSpec xs t

exs :: [Type] -> Parser [Example]
exs tys = do symbol "["
             xs <- sepby (ex tys) (symbol ",")
             symbol "]"
             return xs

ex :: [Type] -> Parser Example
ex tys = do symbol "<"
            xs <- sepby ((do {t <- tm tys; return $ Left t}) <|>
                         (do {symbol "["; t <- ty; symbol "]"; return $ Right t}))
                  (symbol ",")
            symbol ">"
            return $ toExample xs

toExample :: [Either Term Type] -> Example
toExample ((Left tm):[]) = Out tm
toExample ((Left tm):xs) = InTm tm (toExample xs)
toExample ((Right ty):xs) = InTy ty (toExample xs)


{-================================ PARSE DECLS ===============================-}
tyDecl :: Parser TyDecl
tyDecl = do allSpace
            f <- identifier keywords
            symbol "::"
            a <- ty
            return (f,a)

tmDecl :: [Type] -> Parser TmDecl
tmDecl tys = (do allSpace
                 f <- identifier keywords
                 ps <- many (identifier keywords)
                 symbol "="
                 t <- tm tys
                 return (f,ps,Left t)) <|>
             (do allSpace
                 f <- identifier keywords
                 symbol "="
                 xs <- exs tys
                 return (f,[],Right xs))

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
                  tmd <- (tmDecl tys)
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
desugarDecl (Left ((_,ty),(f,as,Left tm))) = [(f,tm')]
  where tm' = tmFromTyTmDecl ty as tm
desugarDecl (Left ((_,ty),(f,as,Right xs))) = [(f,tm')]
  where tm' = TmLSpec xs ty
desugarDecl (Right (ds,tyd,tmd)) = itmd ++ itmtytm
  where itmtytm = desugarDecl $ Left (tyd,tmd)
        itmd = concat $ map tmFromDDecl ds

tmFromDDecl :: DDecl -> [(Id,Term)]
tmFromDDecl (f,(TyTAbs "#R" ty@(TyCase ctys))) = buildConstrs ctys ty


tmFromTyTmDecl :: Type -> [Id] -> Term -> Term
tmFromTyTmDecl (TyAbs ty1 ty2) (a:as) tm = TmAbs a ty1 (tmFromTyTmDecl ty2 as tm)
tmFromTyTmDecl (TyTAbs i ty1) (a:as) tm = TmTAbs i (tmFromTyTmDecl ty1 (a:as) tm)
tmFromTyTmDecl _ _ tm = tm

buildConstr :: (Constr, [Type]) -> Type -> (Id, Term)
buildConstr (c,tys) ty = if tmtys == [(TmVar "a0",TyUnit)]
                         then (c, TmApp (desugarConstr c [] ty) TmUnit)
                         else (c, desugarConstr c [] ty)
  where tys' = map (\x -> if x == (TyVar "#R")
                          then TyTAbs "#R" ty
                          else x) tys
        tms = [TmVar i | i <- vs]
        vs = ["a" ++ show i | i <- [0..(length tys - 1)]]
        tmtys = zip tms tys'

buildConstrs :: [(Constr, [Type])] -> Type -> [(Id,Term)]
buildConstrs ctys ty = map (\x -> buildConstr x ty) ctys
