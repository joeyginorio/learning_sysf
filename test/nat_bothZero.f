data Bool = True
          | False

data Nat = Zero
         | S Nat

and :: Bool -> Bool -> Bool
and = [<True,True,True>, <True,False,False>, <False,True,False>, <False,False,False>]

isZero :: Nat -> Bool
isZero = [<Zero, True>, <S (S Zero), False>]

bothZero :: Nat -> Nat -> Bool
bothZero = [<Zero, Zero, True>, <S (S Zero), Zero, False>, <Zero, S Zero, False>]

main :: Unit
main = bothZero (Zero) (Zero)
