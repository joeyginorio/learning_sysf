data Bool = True
          | False

data BCmd =  BAdd
           | BMul

badd :: Bool -> Bool -> Bool
badd = [<True,True,True>, <True,False,True>, <False,True,True>, <False,False,False>]

bmult :: Bool -> Bool -> Bool
bmult = [<True,True,True>,<True,False,False>,<False,False,False>, <False,True,False>]

beval :: BCmd -> (Bool -> Bool -> Bool)
beval = [<BAdd, badd>,
         <BMul, bmult>]

main :: Unit
main = beval
