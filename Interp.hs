module Interp where

import Grammars

data Value = NumV Int
           | BoolV Bool

instance Show Value where
    show (NumV n) = show n
    show (BoolV b) = if b then "#t" else "#f"

-- Funcion: interp
-- Representa el analisis semantico, toma los arboles de sintaxis
-- abstracta y nos regresa el resultado en el tipo de dato propio de MiniLisp.
interp :: ASA -> Value
interp (Num n) = NumV n
interp (Boolean b) = BoolV b
interp (Op f l)
           | elem f ["+","-","*","/","add1","sub1"] = NumV (interpI (Op f l))
           | elem f ["and","or","not","<",">","="] = BoolV (interpB (Op f l))
           | otherwise = error "operador invalido"

-- Reduce las expresiones del lenguaje a valores numericos del
-- lenguaje anfitrion
interpI :: ASA -> Int
interpI (Num n) = n
interpI (Boolean b) = error "no es numero"
interpI (Op "+" l) = foldr1 (+) (map interpI l)
interpI (Op "-" l) = foldl1 (-) (map interpI l)
interpI (Op "*" l) = foldr1 (*) (map interpI l)
interpI (Op "/" l) = foldl1 (div) (map interpI l)
interpI (Op "add1" l) = (interpI $ head l) + 1
interpI (Op "sub1" l) = (interpI $ head l) - 1

-- Reduce las expresiones del lenguaje a valores booleanos del
-- lenguaje anfitrion
interpB :: ASA -> Bool
interpB (Num n) = error "no es un booleano"
interpB (Boolean b) = b
interpB (Op "and" l) = foldr1 (&&) (map interpB l)
interpB (Op "or" l) = foldr1 (||) (map interpB l)
interpB (Op "not" l) = not(interpB $ head l)
interpB (Op f l) = (orden f (map interpI l))

-- Verifica la condicion ordenal en una lista de numeros dada.
orden :: String -> [Int] -> Bool
orden _ (x:[]) = True
orden "<" (x:y:xs) = (x<y) && (orden "<" (y:xs))
orden ">" (x:y:xs) = (x>y) && (orden ">" (y:xs))
orden "=" (x:y:xs) = (x==y) && (orden "=" (y:xs))
