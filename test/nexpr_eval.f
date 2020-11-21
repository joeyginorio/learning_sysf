data Nat = Zero
         | S Nat

data NExpr = Val Nat
           | NAdd NExpr NExpr
           | NMul NExpr NExpr

plus :: Nat -> Nat -> Nat
plus = [<Zero,Zero,Zero>,<Zero, S Zero, S Zero>, <S Zero, Zero, S Zero>]

mul :: Nat -> Nat -> Nat
mul a b = case a[Nat] of
          Zero -> Zero
          S c -> plus b c

neval :: NExpr -> Nat
neval = [<Val (S Zero), S Zero>,
         <NAdd (Val Zero) (Val Zero), Zero>,
         <NAdd (Val (S Zero)) (Val Zero), S Zero>,
         <NAdd (NAdd (Val Zero) (Val (S Zero))) (NAdd (Val Zero) (Val Zero)), S Zero>,
         <NMul (Val Zero) (Val Zero), Zero>,
         <NMul (Val (S Zero)) (Val Zero), Zero>,
         <NMul (NAdd (Val Zero) (Val (S Zero))) (NAdd (Val (S Zero)) (Val Zero)), (S Zero)>]

main :: Unit
main = neval (NMul (Val (S (S Zero)))  (Val (S (S (S Zero)))))
