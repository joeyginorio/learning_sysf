data Bool = True
          | False

data Nat = Zero
         | S Nat

inc0 :: Nat -> Nat
inc0 = [<Zero,Zero>,<S Zero, S Zero>]

inc1 :: Nat -> Nat
inc1 = [<Zero,S Zero>,<S Zero, S (S Zero)>]

pointwise :: (Nat -> Nat) -> (Nat -> Nat) -> (Nat -> Nat)
pointwise = [<inc0,inc0,inc0>,<inc0,inc1,inc1>,<inc1,inc0,inc1>]

main :: Unit
main = pointwise inc1 inc1 Zero
