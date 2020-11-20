data Bool = True
          | False

and :: Bool -> Bool -> Bool
and = [<True,True,True>,<True,False,False>,<False,True,False>,<False,False,False>]

main :: Unit
main = and
