data Bool = True
          | False

data Nat = Zero
         | S Nat

plus :: Nat -> Nat -> Nat
plus = [<Zero,Zero,Zero>,<Zero, S Zero, S Zero>, <S Zero, Zero, S Zero>]

main :: Unit
main = plus (S Zero) (S Zero)
