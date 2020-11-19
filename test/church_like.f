data Nat = Zero
         | S Nat

convert :: Nat -> (R . (R -> R) -> (Unit -> R) -> R)
convert = [<Zero, forall R.lam c1:(R -> R).lam c0:(Unit -> R).(c0 unit)>,
           <S Zero, forall R.lam c1:(R -> R).lam c0:(Unit -> R).c1 (c0 unit)>,
           <S (S Zero), forall R.lam c1:(R -> R).lam c0:(Unit -> R).c1 (c1 (c0 unit))>]


main :: Unit
main = convert (S (S (S Zero)))
