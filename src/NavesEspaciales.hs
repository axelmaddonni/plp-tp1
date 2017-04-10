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
foldNave fbase fmodulo nave = case nave of
    Base c -> fbase c
    Módulo c n1 n2 -> fmodulo c (foldNave fbase fmodulo n1) (foldNave fbase fmodulo n2)

--Ejercicio 2

capacidad :: NaveEspacial -> Int
capacidad = foldNave (esComponenteX Contenedor) (\x y z -> (esComponenteX Contenedor x)+y+z)

esComponenteX :: Componente -> Componente -> Int
esComponenteX x y = if x == y then 1 else 0

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque nave = cantidadComponenteX Cañón nave

cantidadComponenteX :: Componente -> NaveEspacial -> Int
cantidadComponenteX c = foldNave (esComponenteX c) (\x y z-> (esComponenteX c x) + y + z)

cantidadCañones :: NaveEspacial -> Int
cantidadCañones = cantidadComponenteX Cañón

cantidadEscudos :: NaveEspacial -> Int
cantidadEscudos = cantidadComponenteX Escudo

cantidadContenedores :: NaveEspacial -> Int
cantidadContenedores = cantidadComponenteX Contenedor

cantidadMotores :: NaveEspacial -> Int
cantidadMotores = cantidadComponenteX Motor

puedeVolar :: NaveEspacial -> Bool
puedeVolar = foldNave (==Motor) (\c rIzq rDer -> rIzq || rDer || c==Motor)

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial nave1 nave2 =  cantidadCañones nave1 == cantidadCañones nave2 && cantidadEscudos nave1 == cantidadEscudos nave2 && cantidadMotores nave1 == cantidadMotores nave2  && cantidadContenedores nave1 == cantidadContenedores nave2

--Ejercicio 3

mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = foldr1 (\nave res -> if capacidad nave > capacidad res then nave else res)

--Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar trans = foldNave (Base . trans) (Módulo . trans)

-- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar (d, 0, Grande) (Módulo Escudo n1 n2) = if poderDeAtaque n1 > 0 || poderDeAtaque n2 > 0 then Módulo Escudo n1 n2 else Base Contenedor
impactar (d, 0, Pequeño) (Base Escudo) = Base Escudo
impactar (d, 0, Pequeño) (Módulo Escudo n1 n2 ) = Módulo Escudo n1 n2 
impactar (d, 0, t) n = Base Contenedor    
impactar (d, i, t) (Base c) = Base c 
impactar (d, i, t) (Módulo c n1 n2) = if d == Babor then Módulo c (impactar (d, i-1, t) n1) n2 else Módulo c n1 $impactar (d, i-1, t) n2 

{- No se puede usar foldNave porque para la funcion del caso base requeriría tener noción del Peligro
 y Altura en la que esta el Componente pero esto es imposible
 ya que dicha función solo puede tomar un Componente como parametro de entrada (Componente->b) 
 En realidad lo de arriba no es cierto, ver la implementacion del ejercicio 8.-}

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = foldl (flip impactar)

-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego peligros = filter (puedeVolar . flip maniobrar peligros)

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = foldNave fBase fModulo
    where  fBase = \c -> (\i -> if i == 0 then 1 else 0) 
           fModulo = \c fIzq fDer -> (\i -> if i == 0 then 1 else fIzq (i-1) + fDer (i-1))

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones nave = (altura, maximum (anchos nave altura)) where altura = largo nave

largo :: NaveEspacial -> Int
largo = foldNave (const 1) (\comp largo1 largo2 -> (max largo1 largo2) + 1)

anchos :: NaveEspacial -> Int -> [Int]
anchos nave altura = [componentesPorNivel nave i | i<-[0..altura]]
