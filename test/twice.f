data Nat = Zero
         | S Nat

inc :: Nat -> Nat
inc = [<Zero, S Zero>, <S Zero, S (S Zero)>]

twice :: (Nat -> Nat) -> Nat -> Nat
twice = [<inc,Zero, S (S Zero)>]

main :: Unit
main = twice
