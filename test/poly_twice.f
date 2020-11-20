data Nat = Zero
         | S Nat

inc :: Nat -> Nat
inc = [<Zero, S Zero>, <S Zero, S (S Zero)>]

twice :: X . (X -> X) -> X -> X
twice = [<[Nat],inc,Zero, S (S Zero)>]

main :: Unit
main = twice
