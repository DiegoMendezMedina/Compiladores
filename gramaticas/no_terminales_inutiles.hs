import Data.Set (fromList, toList)
import Data.List 


type Regla = (String, [String])
type Gramatica = ([Regla], String)

-- A -> a | aBc ==> ("A", ["a"]), ("A", ["a", "B", "c"])
-- B -> c       ==> ("B", ["c"])
-- C -> epsilon ==> ("C", [])
-- A es inicial en la gramatica G
-- ==> G = ([("A", ["a"]), ("A", ["a", "B", "c"]), ("B", ["c"]), ("C", [])], "A")

-- | getNoTerminales: Regresa los elementos no terminales de
--                  una gramática.
getNoTerminales :: Gramatica -> [String]
getNoTerminales g = toList. fromList $ getNoTerminalesAux $ fst g

-- | getNoTerminalesAux: Función auxiliar, regresa la primer
--                       proyección. NO regresa un conjunto.
getNoTerminalesAux :: [Regla] -> [String]
getNoTerminalesAux [] = []
getNoTerminalesAux (x:xs) =  fst x : getNoTerminalesAux xs

-- | inutiles: Regresa los elementos no terminales
--             inutiles de una gramática.
inutiles :: Gramatica -> [String]
inutiles g = (getNoTerminales g) \\ utiles g

-- | utiles: Regresa los elementos no terminales utiles de una grámatica.
--           Es decir, los que son alcanzados por la gramática.
utiles :: Gramatica -> [String]
utiles g = (snd g) : (l ++ (utilesAux g  l))
  where l = noTerminalesVisitados g

-- | utilesAux: Permite ir actualizando los utiles en cuanto
--              /visitamos/ otro no terminal y hace recursión sobre la lista dada.
--  gramatica -> elementos no terminales utiles iniciales -> utiles finales
utilesAux :: Gramatica -> [String] -> [String]
utilesAux _ [] = []
utilesAux g (x:xs) = nl ++ (utilesAux g (xs++nl))
  where nl = intersect (noTerminalesVisitadosAux (fst g) [] x)
          (getNoTerminales g)

-- | noTerminalesVisitados: Nos regresa los elementos no terminales
--                        que aparecen en reglas donde el lado izquierdo
--                        es el inicial de la gramática.
noTerminalesVisitados :: Gramatica -> [String]
noTerminalesVisitados g = intersect (noTerminalesVisitadosAux (fst g) [] (snd g))
  (getNoTerminales g)

-- | noTerminalesVisitadosAux: Elementos no terminales que aparecen en
--                             una regla dada. Recursión de cola
-- reglas -> solución constructiva -> no terminal inicial -> respuesta
noTerminalesVisitadosAux :: [Regla] -> [String] -> String -> [String]
noTerminalesVisitadosAux [] s _ = s
noTerminalesVisitadosAux (x:xs) s i
  | i == fst x = noTerminalesVisitadosAux xs ((snd x)++s) i
  | otherwise  = noTerminalesVisitadosAux xs s i

---
-- Ejemplos
---

-- inutiles(g) = {E}
g = ([("S", ["a"]),("S", ["a","A","e"]), ("E", ["0"]), ("A", ["I"]) ,("I", ["o"])], "S")
-- inutiles(g1) = {A, E, I}
g1 = ([("S", ["a"]),("A", ["a","A","e"]), ("E", ["0"]), ("A", ["I"]) ,("I", ["o"])], "S")
-- inutiles(g2) = {}
g2 = ([("I", ["o"]), ("S", ["A"]),("A", ["a","E","e"]), ("E", ["0"]), ("E", ["I"])], "S")
