data Nat = Zero
         | S Nat

plus :: Nat -> Nat -> Nat
plus = [<Zero,Zero,Zero>,<Zero, S Zero, S Zero>, <S Zero, Zero, S Zero>]

mul :: Nat -> Nat -> Nat
mul = [<Zero,Zero,Zero>,<Zero, Zero, S Zero>, <S Zero, Zero, Zero>,
       <S Zero, S Zero, S Zero>,<S Zero, S (S Zero), S (S Zero)>, <S (S Zero), S Zero, S (S Zero)>,
       <S (S Zero), S (S (S Zero)), S (S (S (S (S (S Zero)))))>]

main :: Unit
main = mul (S (S (S Zero))) (S (S (S (S Zero))))
