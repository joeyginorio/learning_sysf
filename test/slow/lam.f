data Bool = True
          | False

data Expr = Cst Bool | Lam (Bool -> Bool) | App Expr Expr

not :: Bool -> Bool
not = [<True,False>, <False,True>]

lift :: Bool -> Expr
lift = [<True, Cst True>, <False, Cst False>]

eval :: Expr -> Bool
eval = [<Cst True, True>, <Cst False,False>,
        <App (Lam (lam b:Bool.b)) (Cst True),True>,
        <App (Lam (lam b:Bool.not b)) (Cst True),False>]

main :: Unit
main = True
