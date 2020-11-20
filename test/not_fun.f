data Bool = True
          | False

not :: Bool -> Bool
not = [<True,False>, <False,True>]

apply :: (Bool -> Bool) -> Bool -> Bool
apply = [<lam b:Bool.b, True, True>, <lam b:Bool.b, False, False>,
         <lam b:Bool.not b, True, False>, <lam b:Bool.not b, False, True>]

main :: Unit
main = apply
