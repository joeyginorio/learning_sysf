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
appFun = [<BoolFun (lam x:Bool.x),BoolData False,BoolData False>,
          <BoolFun (lam x:Bool.x),BoolData True,BoolData True>,
          <BoolFun (lam x:Bool.x),NatData Zero,BoolData False>,
          <BoolFun (lam x:Bool.x),NatData (S Zero),BoolData True>,
          <BoolFun (lam x:Bool.not x),BoolData False,BoolData True>,
          <BoolFun (lam x:Bool.not x),BoolData True,BoolData False>,
          <BoolFun (lam x:Bool.not x),NatData Zero,BoolData True>,
          <BoolFun (lam x:Bool.not x),NatData (S Zero),BoolData False>,
          <NatFun (lam x:Nat.x),BoolData False,NatData Zero>,
          <NatFun (lam x:Nat.x),BoolData True,NatData (S Zero)>,
          <NatFun (lam x:Nat.x),NatData Zero,NatData Zero>,
          <NatFun (lam x:Nat.x),NatData (S Zero),NatData (S Zero)>,
          <NatFun (lam x:Nat.inc x),BoolData False,NatData (S Zero)>,
          <NatFun (lam x:Nat.inc x),BoolData True,NatData (S (S Zero))>,
          <NatFun (lam x:Nat.inc x),NatData Zero,NatData (S Zero)>,
          <NatFun (lam x:Nat.inc x),NatData (S Zero),NatData (S (S Zero))>]


main :: Unit
main = True

