module Interp where

import Grammars
import Desugar

data Value = NumV Int
           | BoolV Bool

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = if b then "#t" else "#f"

-- interp : Representa el analisis semantico, toma los arboles de sintaxis
-- abstracta y nos regresa el resultado en el tipo de dato propio de MiniLisp.
interp :: ASA -> Value
interp (Id i) = error "Variable no definida."
interp (Num n) = (NumV n)
interp (Boolean b) = (BoolV b)
interp (Uop f e)
         | f == "not" = BoolV (interpB (Uop f e))
         | otherwise = NumV (interpI (Uop f e))
interp (Binop f i d)
         | elem f ["add1", "sub1", "+", "-", "*", "/"] = NumV (interpI (Binop f i d))
         | otherwise = BoolV (interpB (Binop f i d))
interp (Let [x] c) = interp (sust c x)

-- sust : Se define la sustitución sobre la sintaxis abstracta del lenguaje.
sust :: ASA -> Binding -> ASA
sust (Id i) (x,e) = if i==x then e else (Id i)
sust (Num n) _ = (Num n)
sust (Boolean b) _ = (Boolean b)
sust (Uop f a) b = (Uop f (sust a b))
sust (Binop f i d) b = (Binop f (sust i b) (sust d b))
sust (Let ((i,v):ls) c) (x,e)
            | i == x = (Let ((i,(sust v (x,e))):ls) c)
            | otherwise = (Let ((i,(sust v (x,e))):ls) (sust c (x,e)))

interpI :: ASA -> Int
interpI (Num n) = n
interpI (Uop "add1" e) = (interpI e ) + 1
interpI (Uop "sub1" e) = (interpI e ) - 1
interpI (Binop "+" i d) = (interpI i) + (interpI d)
interpI (Binop "-" i d) = (interpI i) - (interpI d)
interpI (Binop "*" i d) = (interpI i) * (interpI d)
interpI (Binop "/" i d) = div (interpI i) (interpI d)

interpB :: ASA -> Bool
interpB (Boolean b) = b
interpB (Uop "not" e) = not(interpB e)
interpB (Binop "and" i d) = (interpB i) && (interpB d)
interpB (Binop "or" i d) = (interpB i) || (interpB d)
interpB (Binop f i d) = (orden f (reduce (Binop f i d)))

reduce :: ASA -> [Int]
reduce (Binop _ (Num n) (Num m)) = [n,m]
reduce (Binop _ (Num n) d) = n:(reduce d)

orden :: String -> [Int] -> Bool
orden _ (x:[]) = True
orden "<" (x:y:xs) = (x<y) && (orden "<" (y:xs))
orden ">" (x:y:xs) = (x>y) && (orden ">" (y:xs))
orden "=" (x:y:xs) = (x==y) && (orden "=" (y:xs))
