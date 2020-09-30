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

tm ::= lam id : ty . tm | tm tm | x | tt | ff | unit | (tm)
ty ::= Bool | Unit | ty -> ty | (type)

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

type Decl = (TyDecl, TmDecl)
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
tyAtom = tyUnit <|> tyBool <|> parens ty

ty :: Parser Type
ty = tyAtom `chainr1` tyAbsOp


{-================================ PARSE TERMS ===============================-}

keywords :: [String]
keywords = ["let", "in", "lam"]

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

-- tmLet :: Parser Term
-- tmLet = do  symbol "let"
--             x <- identifier keywords
--             symbol "="
--             t1 <- tm
--             symbol "in"
--             t2 <- tm
--             return $ TmLet x t1 t2

tmAtom :: Parser Term
tmAtom = tmAbs <|> tmUnit <|> tmTrue <|> tmFalse <|> tmVar <|>
          parens tm

tmAppOp :: Parser (Term -> Term -> Term)
tmAppOp = return TmApp

tm :: Parser Term
tm = do tmAtom `chainl1` tmAppOp


{-================================ PARSE DECLS ===============================-}
tydecl :: Parser TyDecl
tydecl = do allSpace
            f <- identifier keywords
            symbol "::"
            a <- ty
            return (f,a)

tmdecl :: Parser TmDecl
tmdecl = do allSpace
            f <- identifier keywords
            ps <- many (identifier keywords)
            symbol "="
            t <- tm
            return (f,ps,t)

decl :: Parser Decl
decl = do tyd <- tydecl
          tmd <- tmdecl
          return (tyd,tmd)


{-================================ PARSE PROGS ===============================-}
prog :: Parser Prog
prog = do p <- many decl
          return p

foo = "\n\
\f :: Bool -> Bool -> Bool -> Bool\n\
\f x y z = tt\n\
\\n\
\g :: Unit -> Bool\n\
\g w v = ff"

parseFile :: String -> Parser a -> IO a
parseFile f p = fmap (fst . head . parse p) (readFile f)
