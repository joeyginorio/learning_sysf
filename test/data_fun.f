data Bool = True
          | False

data Nat = Zero
         | S Nat

data Fun = BoolFun (Bool -> Bool)
         | NatFun (Nat -> Nat)

data Data = BoolData Bool
          | NatData Nat

not :: Bool -> Bool
not = [<True,False>, <False,True>]

inc :: Nat -> Nat
inc = [<Zero, S Zero>, <S Zero, S (S Zero)>]

bool2nat :: Bool -> Nat
bool2nat = [<False,Zero>,<True,S Zero>]

nat2bool :: Nat -> Bool
nat2bool = [<Zero,False>,<S Zero,True>,<S (S Zero),True>]

appFun :: Fun -> Data -> Data
appFun = [<BoolFun (lam x:Bool.x),BoolData True,BoolData True>,
          <BoolFun (lam x.Bool.x),NatData (S Zero),BoolData True>,
          <NatFun inc, NatData (S Zero), NatData (S (S Zero))>,
          <NatFun inc, BoolData False, NatData (S Zero)>]

main :: Unit
main = appFun (NatFun inc) (BoolData True)

