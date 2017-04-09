module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq
	
data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . (padNave 0 0 False)
  
padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++ 
            pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
            padNave (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1

foldNave :: (Componente->b) -> (Componente->b->b->b) -> NaveEspacial -> b
foldNave f g nave = case nave of
	Base x -> f x
	Módulo x y z -> g x (foldNave f g y) (foldNave f g z)

--Ejercicio 2

capacidad :: NaveEspacial -> Int
capacidad = foldNave (esComponenteX Contenedor) (\x y z -> (esComponenteX Contenedor x)+y+z)

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque nave = cantidadComponenteX Cañón nave

cantidadComponenteX :: Componente -> NaveEspacial -> Int
cantidadComponenteX c = foldNave (esComponenteX c) (\x y z-> (esComponenteX c x) + y + z)

esComponenteX :: Componente -> Componente -> Int
esComponenteX x y = if x == y then 1 else 0

puedeVolar :: NaveEspacial -> Bool
puedeVolar = foldNave (==Motor) (\c rIzq rDer -> rIzq || rDer || c==Motor)

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial nave1 nave2 =  cantidadComponenteX Cañón nave1 == cantidadComponenteX Cañón nave2 && cantidadComponenteX Escudo nave1 == cantidadComponenteX Escudo nave2 && cantidadComponenteX Motor nave1 == cantidadComponenteX Motor nave2  && cantidadComponenteX Contenedor nave1 == cantidadComponenteX Contenedor nave2

--Ejercicio 3

mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = foldr1 (\nave res -> if capacidad nave > capacidad res then nave else res)

--Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar = undefined

-- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar p nave = undefined

-- Definir la funcion impactar :: Peligro -> NaveEspacial -> NaveEspacial
-- que devuelve la nave resultante luego de enfrentar el peligro correspondiente
-- (segun se detallo en la seccion “Peligros”)
-- Para este ejercicio puede utilizarse recursion explicita. Se debe explicar en un comentario por
-- que el esquema foldNave no es adecuado para esta funcion

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = undefined

-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego = undefined

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = undefined

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = undefined
