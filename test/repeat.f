data Nat = Zero
         | S Nat

data Bool = True
          | False

not :: Bool -> Bool
not = [<True,False>, <False,True>]

inc :: Nat -> Nat
inc = [<Zero, S Zero>, <S Zero, S (S Zero)>]

repeat :: X. Nat -> (X -> X) -> X -> X
repeat = [<[Nat],Zero,inc,Zero, Zero>,
          <[Nat],S Zero,inc,Zero, S Zero>,
          <[Nat],S (S Zero),inc,Zero, S (S Zero)>,
          <[Bool],Zero,not,False, False>,
          <[Bool],S Zero,not,False, True>,
          <[Bool],S (S Zero),not,False, False>]

main :: Unit
main = repeat [Nat] (S (S (S Zero))) inc (S (S Zero))
