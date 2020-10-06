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

tmAbs :: Parser Term
tmAbs = do  symbol "lam"
            x <- identifier keywords
            symbol ":"
            a <- ty
            symbol "."
            t <- tm
            return $ TmAbs x a t

tmLet :: Parser Term
tmLet = do symbol "let"
           itms <- many (do  x <- identifier keywords
                             symbol "="
                             t <- tm
                             line
                             return (x,t))
           symbol "in"
           t <- tm
           return $ TmLet itms t

tmConstr :: Parser Term
tmConstr = do c <- constructor
              tms <- many (tmVar <|> tm)
              symbol "as"
              a <- ty
              return $ TmConstr c tms a

tmCase :: Parser Term
tmCase = do symbol "case"
            t <- tm
            symbol "of"
            symbol "\n" <|> (symbol "")
            tmtms <- many (do t1 <- tm
                              symbol "->"
                              t2 <- tm
                              (symbol ";" <|> symbol "\n" <|> symbol "")
                              return (t1,t2))
            return $ TmCase t tmtms

tmAtom :: Parser Term
tmAtom = tmCase <|> tmLet <|> tmAbs <|> tmUnit <|> tmTrue <|> tmFalse <|>
         tmConstr <|> tmVar <|> parens tm

tmAppOp :: Parser (Term -> Term -> Term)
tmAppOp = return TmApp

tm :: Parser Term
tm = do tmAtom `chainl1` tmAppOp


{-================================ PARSE DECLS ===============================-}
tyDecl :: Parser TyDecl
tyDecl = do allSpace
            f <- identifier keywords
            symbol "::"
            a <- ty
            return (f,a)

tmDecl :: Parser TmDecl
tmDecl = do allSpace
            f <- identifier keywords
            ps <- many (identifier keywords)
            symbol "="
            t <- tm
            return (f,ps,t)

dDecl :: Parser DDecl
dDecl = do symbol "data"
           d <- constructor
           symbol "="
           c1 <- constructor
           tys1 <- many ty
           ctys <- many (do symbol "\n" <|> (symbol "")
                            symbol "|"
                            c <- constructor
                            tys <- many ty
                            return (c,tys))
           return (d, (TyCase ((c1,tys1):ctys)))


tytmDecl :: Parser Decl
tytmDecl = do tyd <- tyDecl
              tmd <- tmDecl
              return $ Left (tyd,tmd)

dtytmDecl :: Parser Decl
dtytmDecl = do dds <- many (do d <- dDecl
                               many line
                               return d)
               tyd <- tyDecl
               tmd <- tmDecl
               return $ Right (dds ,tyd,tmd)

decl :: Parser Decl
decl = tytmDecl <|> dtytmDecl

{-================================ PARSE PROGS ===============================-}
prog :: Parser Prog
prog = do p <- many decl
          return p

parseFile :: String -> Parser a -> IO a
parseFile f p = fmap (fst . head . parse p) (readFile f)

-- Similar to parseFile, but includes info about parse for debugging.
parseFile' :: String -> Parser a -> IO [(a,String)]
parseFile' f p = fmap (parse p) (readFile f)


{-============================== DESUGAR PROGS ===============================-}
-- FIX, DTYPES ALSO ADD CONSTRUCTORS TO THE CONTEXT
desugarProg :: Prog -> Term
desugarProg (d:[]) = desugarDecl d
desugarProg (d@(Left ((f,_),_)):ds) = TmLet [(f,tm1)] tm2
  where tm1 = desugarDecl d
        tm2 = desugarProg ds
desugarProg (d@(Right (_,(f,_),_)):ds) = TmLet [(f,tm1)] tm2
  where tm1 = desugarDecl d
        tm2 = desugarProg ds

desugarDecl :: Decl -> Term
desugarDecl (Left ((_,_),(_,[],tm))) = tm
desugarDecl (Left ((_,(TyAbs ty1 ty2)),(f,(a:as),tm))) =
  TmAbs a ty1 $ desugarDecl (Left ((f,ty2),(f,as,tm)))
desugarDecl (Right ([],tyd,tmd)) = desugarDecl $ Left (tyd, tmd)
desugarDecl (Right (((f,ty@(TyCase ctys)):ds),tyd,tmd)) = TmTApp (TmTAbs f tm) ty
  where tm = TmLet itms (desugarDecl (Right (ds,tyd,tmd)))
        itms = buildConstrs ctys ty


-- desugarTm :: Term -> F.Term

-- desugarTy :: Type -> F.Type

buildConstr :: (Constr, [Type]) -> Type -> (Id, Term)
buildConstr (c,tys) ty = (c, foldr f (TmConstr c tms ty) tmtys)
  where tms = [TmVar i | i <- vs]
        vs = ["c" ++ show i | i <- [0..(length tys - 1)]]
        f = (\((TmVar i),ty) a -> TmAbs i ty a)
        tmtys = zip tms tys

buildConstrs :: [(Constr, [Type])] -> Type -> [(Id,Term)]
buildConstrs ctys ty = map (\x -> buildConstr x ty) ctys

