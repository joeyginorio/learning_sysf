data Bool = True
          | False

nand :: Bool -> Bool -> Bool
nand = [<True,True,False>,<True,False,True>,<False,True,True>,<False,False,True>]

main :: Unit
main = nand
