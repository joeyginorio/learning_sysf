data Bool = True
          | False

data BExpr = Val Bool
           | BAdd BExpr BExpr
           | BMul BExpr BExpr

badd :: Bool -> Bool -> Bool
badd = [<True,True,True>, <True,False,True>, <False,True,True>, <False,False,False>]

bmult :: Bool -> Bool -> Bool
bmult = [<True,True,True>,<True,False,False>,<False,False,False>, <False,True,False>]

beval :: BExpr -> Bool
beval = [<Val True, True>,
         <BAdd (Val False) (Val False), False>,
         <BAdd (Val True) (Val False), True>,
         <BAdd (BAdd (Val False) (Val True)) (BAdd (Val False) (Val False)), True>,
         <BMul (Val False) (Val False), False>,
         <BMul (Val True) (Val False), False>,
         <BMul (BAdd (Val False) (Val True)) (BAdd (Val True) (Val False)), True>]

main :: Unit
main = beval
