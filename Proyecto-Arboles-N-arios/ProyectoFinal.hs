module ProyectoFinal where

-- =================================================================
-- 0. FUNCIONES AUXILIARES
-- =================================================================

-- Nuestra version de 'map'
ourMap :: (a -> b) -> [a] -> [b]
ourMap _ [] = []
ourMap f (x:xs) = f x : ourMap f xs

-- Nuestra version de 'sum'
ourSuma :: Num a => [a] -> a
ourSuma [] = 0
ourSuma (x:xs) = x + ourSuma xs

-- Nuestra version de 'max'
ourMaximo :: Ord a => [a] -> a
ourMaximo [x] = x
ourMaximo (x:xs) = mayor x (ourMaximo xs)
  where 
    mayor a b = if a > b then a else b

-- Nuestra version de 'reverse'
ourReversa :: [a] -> [a]
ourReversa [] = []
ourReversa (x:xs) = ourReversa xs ++ [x]

-- Nuestra version de 'elem'
ourElem :: Eq a => a -> [a] -> Bool
ourElem _ [] = False
ourElem buscado (x:xs)
    | buscado == x = True
    | otherwise    = ourElem buscado xs

-- Nuestra version de 'concat'
ourConcat :: [[a]] -> [a]
ourConcat [] = []
ourConcat (x:xs) = x ++ ourConcat xs

-- =================================================================
-- PARTE 1: IMPLEMENTACION DEL TIPO DE DATO
-- =================================================================
data NTree a = Node a [NTree a]
  deriving (Eq)

instance Show a => Show (NTree a) where
    show t = mostrar t 0
      where
        mostrar (Node x hijos) nivel =
            replicate (nivel * 2) ' ' ++ show x ++ "\n" ++
            ourConcat (ourMap (\h -> mostrar h (nivel + 1)) hijos)

-- =================================================================
-- PARTE 2: ARBOLES DE SINTAXIS ABSTRACTA (LOGICA)
-- =================================================================

data Prop = Var String | Cons Bool | Not Prop | And Prop Prop | Or Prop Prop | Impl Prop Prop | Syss Prop Prop
    deriving (Eq)

instance Show Prop where
    show (Var p) = p
    show (Cons b) = show b
    show (Not p) = "¬" ++ show p
    show (And p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

type Estado = [String]

-- 2.1 Formula -> Arbol
propToTree :: Prop -> NTree String
propToTree (Var x)    = Node x []
propToTree (Cons b)   = Node (show b) []
propToTree (Not p)    = Node "¬" [propToTree p]
propToTree (And p q)  = Node "^" [propToTree p, propToTree q]
propToTree (Or p q)   = Node "v" [propToTree p, propToTree q]
propToTree (Impl p q) = Node "->" [propToTree p, propToTree q]
propToTree (Syss p q) = Node "<->" [propToTree p, propToTree q]

-- 2.2 Arbol -> Formula 
treeToProp :: NTree String -> Prop
treeToProp (Node "¬" (h:[]))      = Not (treeToProp h)
treeToProp (Node "^" (h1:h2:[]))  = And (treeToProp h1) (treeToProp h2)
treeToProp (Node "v" (h1:h2:[]))  = Or (treeToProp h1) (treeToProp h2)
treeToProp (Node "->" (h1:h2:[])) = Impl (treeToProp h1) (treeToProp h2)
treeToProp (Node "<->" (h1:h2:[]))= Syss (treeToProp h1) (treeToProp h2)
treeToProp (Node "True" [])       = Cons True
treeToProp (Node "False" [])      = Cons False
treeToProp (Node x [])            = Var x
treeToProp _                      = error "Estructura invalida"

-- 2.3 Evaluacion
evalTree :: NTree String -> Estado -> Bool
evalTree (Node str hijos) estado
    | str == "¬" = not (evalTree (cabeza hijos) estado)
    | str == "^" = evalTree (cabeza hijos) estado && evalTree (cabeza (cola hijos)) estado
    | str == "v" = evalTree (cabeza hijos) estado || evalTree (cabeza (cola hijos)) estado
    | str == "->" = not (evalTree (cabeza hijos) estado) || evalTree (cabeza (cola hijos)) estado
    | str == "<->" = evalTree (cabeza hijos) estado == evalTree (cabeza (cola hijos)) estado
    | str == "True" = True
    | str == "False" = False
    | otherwise     = ourElem str estado
  where
    cabeza (x:_) = x
    cola (_:xs) = xs

-- =================================================================
-- PARTE 3: OTRAS FUNCIONES
-- =================================================================

-- 3.1 Cantidad de Elementos
cantidadElementos :: NTree a -> Int
cantidadElementos (Node _ hijos) = 1 + ourSuma (ourMap cantidadElementos hijos)

-- 3.3 Suma de Elementos
sumaElementos :: Num a => NTree a -> a
sumaElementos (Node x hijos) = x + ourSuma (ourMap sumaElementos hijos)

-- 3.5 Altura
altura :: NTree a -> Int
altura (Node _ []) = 0
altura (Node _ hijos) = 1 + ourMaximo (ourMap altura hijos)

-- 3.6 Espejo
espejo :: NTree a -> NTree a
espejo (Node x hijos) = Node x (ourReversa (ourMap espejo hijos))

-- 3.4 Preorden
preorden :: NTree a -> [a]
preorden (Node x hijos) = x : ourConcat (ourMap preorden hijos)
