data Bool = True
          | False

or :: Bool -> Bool -> Bool
or = [<True,True,True>,<True,False,True>,<False,True,True>,<False,False,False>]

main :: Unit
main = or
