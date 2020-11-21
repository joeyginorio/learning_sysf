data Nat = Zero
         | S Nat

data Bool = True
          | False

not :: Bool -> Bool
not = [<True,False>, <False,True>]

inc :: Nat -> Nat
inc = [<Zero, S Zero>, <S Zero, S (S Zero)>]

bool2nat :: Bool -> Nat
bool2nat = [<False,Zero>,<True,S Zero>]

nat2bool :: Nat -> Bool
nat2bool = [<Zero,False>,<S Zero,True>,<S (S Zero),True>]

compose :: X. Y. Z. (X -> Y) -> (Y -> Z) -> (X -> Z)
compose = [<[Nat],[Nat],[Nat],inc,inc,S Zero,S (S (S Zero))>,
           <[Nat],[Bool],[Nat],nat2bool,bool2nat,S (S Zero),S Zero>]

main :: Unit
main = compose [Bool] [Nat] [Bool] bool2nat nat2bool True
