data Bool = True
          | False

xor :: Bool -> Bool -> Bool
xor = [<True,True,False>,<True,False,True>,<False,True,True>,<False,False,False>]

main :: Unit
main = xor
