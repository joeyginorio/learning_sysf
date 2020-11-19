data Bool = True
          | False

not :: Bool -> Bool
not = [<True,False>, <False,True>]

applyNot :: (Bool -> Bool) -> Bool -> Bool
applyNot = [<lam b:Bool.b, True, False>, <lam b:Bool.b, False, True>,
            <lam b:Bool.not b, True, True>, <lam b:Bool.not b, False, False>]

main :: Unit
main = applyNot (lam b:Bool.b) False
