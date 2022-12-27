module Desugar where

import Grammars

data ASA = Id String
         | Num Int
         | Boolean Bool
         | Uop String ASA
         | Binop String ASA ASA
         | Let [Binding] ASA
         deriving(Show)

type Binding = (String,ASA)

-- debinding: Funcion auxiliar
--            Convierte (String,ASAS) a (String,ASA)
debinding :: BindingS -> Binding
debinding (i, e) = (i, desugar e)

-- desugar : Se procede a detectar expresiones que puedan ser equivalentes
--           para evitar hacer un doble análisis en la especificación
--           de la semántica.
desugar :: ASAS -> ASA
desugar (IdS i) = (Id i)
desugar (NumS n) = (Num n)
desugar (BooleanS b) = (Boolean b)
desugar (OpS f l) = operaciones (OpS f l)
desugar (LetS l c) = asignaciones (LetS l c)
desugar (LetStar l c) = asignaciones (LetStar l c)

-- asignaciones: Convierte expresiones let versión endulzada
--               a expresiones let anidadas.
asignaciones :: ASAS -> ASA
asignaciones (LetS [x] c) = (Let [debinding x] (desugar c))
asignaciones (LetS (x:xs) c) = (Let [debinding x] (asignaciones (LetS xs c)))
asignaciones (LetStar [x] c) = (Let [debinding x] (desugar c))
asignaciones (LetStar (x:xs) c) = (Let [debinding x] (asignaciones (LetStar xs c)))


-- operaciones: Las operaciones pueden traducirse a
--              expresiones de aridad 1 o 2 respectivamente.
operaciones :: ASAS -> ASA
operaciones (OpS "not" [x]) = (Uop "not" (desugar x))
operaciones (OpS "add1" [x]) = (Uop "add1" (desugar x))
operaciones (OpS "sub1" [x]) = (Uop "sub1" (desugar x))
operaciones (OpS f (x:y:[])) = (Binop f (desugar x) (desugar y))
operaciones (OpS f (x:xs)) = (Binop f (desugar x) (operaciones (OpS f xs)))
