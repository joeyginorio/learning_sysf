data Bool = True
          | False

not :: Bool -> Bool
not = [<True,False>, <False,True>]

main :: Unit
main = not (False)
