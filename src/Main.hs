module Main where
import NavesEspaciales
import Test.HUnit
import Data.List

--Naves para pruebas:
contenedorSolo = Base Contenedor
nave1 = Base Motor
nave2 = Módulo Cañón (Base Escudo) (Base Motor)
nave3 = Módulo Motor (Base Escudo) (Base Cañón)
nave4 = Módulo Contenedor nave2 nave3
nave5 = Módulo Contenedor nave3 nave2
nave5Espejo = Módulo Contenedor nave2 nave3
nave6 = Módulo Contenedor nave4 nave1
nave7 = Módulo Contenedor nave1 nave5
nave8 = Módulo Contenedor nave1 nave6
nave9 = Módulo Escudo 
    (Módulo Escudo (Módulo Escudo (Base Escudo) (Base Cañón)) (Módulo Motor (Base Contenedor) (Base Motor))) 
    (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))
nave9transformada = Módulo Contenedor 
    (Módulo Contenedor (Módulo Contenedor (Base Contenedor) (Base Escudo)) (Módulo Motor (Base Cañón) (Base Motor))) 
    (Módulo Contenedor (Módulo Cañón (Base Motor) (Base Cañón)) (Módulo Contenedor (Base Escudo) (Base Contenedor)))

nave9semiPartida =  Módulo Escudo 
    (Módulo Escudo (Base Contenedor) (Módulo Motor (Base Contenedor) (Base Motor))) 
    (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))

nave9Partida =  Módulo Escudo 
    (Base Contenedor) 
    (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))

soloUnMotor = Base Motor
puroContenedor = Módulo Contenedor (Base Contenedor) (Base Contenedor)
tresCañones = Módulo Cañón (Base Cañón) (Base Cañón)

contenedorYCañon = Módulo Contenedor (Base Cañón) (Base Contenedor)
otroCañon = Módulo Contenedor (Base Contenedor) (Base Cañón)

escudoSinCañon = Módulo Escudo (Base Contenedor) (Base Contenedor)

protegido = Módulo Escudo (Base Contenedor) (Base Cañón)
protegidoNivel1Estribor = Módulo Contenedor soloUnMotor protegido

superProtegido = Módulo Motor protegido protegido
superPartido = Módulo Motor (Base Contenedor) protegido
superDestruido = Módulo Motor (Base Contenedor) (Base Contenedor)

desbalanceado = Módulo Escudo (Base Contenedor) protegido

impactable = Módulo Contenedor (Módulo Escudo (Base Cañón) (Base Motor)) (Base Motor)
impactado1B = Módulo Contenedor (Base Contenedor) (Base Motor)

-- Funciones de Transformacion

transformador :: Componente -> Componente
transformador Cañón = Escudo
transformador Motor = Motor
transformador Escudo = Contenedor
transformador Contenedor = Cañón 

aEscudos :: Componente -> Componente
aEscudos _ = Escudo

-- Constructor de Naves

constructorDeNavesEscudo :: Int -> NaveEspacial
constructorDeNavesEscudo n = (foldr (\f y -> f y) (Base Escudo)) [agregarX Escudo | i <- [1..n]]

agregarX :: Componente -> NaveEspacial -> NaveEspacial
agregarX componente nave = Módulo componente nave nave


-- Peligros

peligroFulminanteBabor = (Babor,0,Torpedo)
peligroFulminanteEstribor = (Babor,0,Torpedo)

peligroErradoEstribor = (Estribor,100,Torpedo)
peligroErradoBabor = (Babor,100,Torpedo)

--Ejecución de los tests

main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? capacidad soloUnMotor,
  1 ~=? capacidad nave4,
  2 ~=? capacidad nave6,
  3 ~=? capacidad nave8,
  3 ~=? capacidad puroContenedor,
  0 ~=? poderDeAtaque soloUnMotor,
  0 ~=? poderDeAtaque puroContenedor,
  3 ~=? poderDeAtaque tresCañones,
  1 ~=? poderDeAtaque contenedorYCañon,
  True ~=? puedeVolar soloUnMotor,
  False ~=? puedeVolar tresCañones,
  True ~=? puedeVolar superProtegido,
  False ~=? puedeVolar desbalanceado,
  True ~=? puedeVolar nave5,
  True ~=? mismoPotencial nave4 nave5,
  False ~=? mismoPotencial superProtegido superPartido,
  True ~=? mismoPotencial nave5 nave5Espejo
  ]

testsEj3 = test [
  nave4 ~=? mayorCapacidad [nave4],
  nave6 ~=? mayorCapacidad [nave6, nave1],
  nave6 ~=? mayorCapacidad [nave4, nave6],
  nave8 ~=? mayorCapacidad [soloUnMotor, nave8, nave6, nave4],
  puroContenedor ~=? mayorCapacidad [soloUnMotor, puroContenedor],
  puroContenedor ~=? mayorCapacidad [nave8, puroContenedor]
  ]

testsEj4 = test [
  soloUnMotor ~=? transformar transformador soloUnMotor,
  tresCañones ~=? transformar transformador puroContenedor,
  Módulo Escudo (Base Contenedor) (Base Motor) ~=? transformar transformador nave2,
  nave9transformada ~=? transformar transformador nave9,

  constructorDeNavesEscudo 0 ~=? transformar aEscudos nave1,
  constructorDeNavesEscudo 1 ~=? transformar aEscudos nave2,
  constructorDeNavesEscudo 1 ~=? transformar aEscudos nave3,
  constructorDeNavesEscudo 2 ~=? transformar aEscudos nave4,
  constructorDeNavesEscudo 2 ~=? transformar aEscudos nave5,
  constructorDeNavesEscudo 3 ~=? transformar aEscudos nave9

  ]

testsEj5 = test [
  impactado1B ~=? impactar (Babor, 1, Torpedo) impactable,
  impactable ~=? impactar (Babor, 1, Grande) impactable,
  impactable ~=? impactar (Babor, 1, Pequeño) impactable,
  (Base Contenedor) ~=? impactar (Babor, 0, Pequeño) impactable,
  superProtegido ~=? impactar (Estribor, 1, Pequeño) superProtegido,
  superProtegido ~=? impactar (Babor, 1, Pequeño) superProtegido,
  superProtegido ~=? impactar (Babor, 1, Grande) superProtegido,
  superProtegido ~=? impactar (Estribor, 1, Grande) superProtegido,
  superPartido ~=? impactar (Babor, 1, Torpedo) superProtegido,
  superPartido ~=? impactar (Estribor, 1, Grande) superPartido,
  superPartido ~=? impactar (Babor, 1, Grande) superPartido,
  superDestruido ~=? impactar (Estribor, 1, Torpedo) superPartido,
  contenedorSolo ~=? impactar (Estribor, 0, Pequeño) superDestruido
  ]

testsEj6 = test [
  impactable ~=? maniobrar impactable [],
  impactable ~=? maniobrar impactable [(Babor, 1, Grande)],
  impactado1B ~=? maniobrar impactable [(Babor, 1, Torpedo)],
  impactado1B ~=? maniobrar impactable [(Babor, 1, Grande), (Babor, 1, Pequeño), (Babor, 1, Torpedo)],
  impactado1B ~=? maniobrar impactable [(Babor, 1, Grande), (Babor, 1, Pequeño), (Babor, 1, Torpedo), (Babor, 1, Pequeño)],
  contenedorSolo ~=? maniobrar impactable [(Babor, 1, Grande), (Babor, 0, Pequeño)],
  contenedorSolo ~=? maniobrar impactable [(Babor, 1, Grande), (Babor, 0, Pequeño), (Babor, 1, Pequeño)],
  superProtegido ~=? maniobrar superProtegido [(Estribor, 1, Pequeño), (Babor, 1, Grande), (Estribor, 1, Grande), (Babor, 1, Pequeño)],
  superPartido ~=? maniobrar superProtegido [(Estribor, 1, Pequeño), (Babor, 1, Grande), (Babor, 1, Torpedo), (Estribor, 1, Grande), (Babor, 1, Grande)],
  superDestruido ~=? maniobrar superProtegido [(Estribor, 1, Pequeño), (Babor, 1, Grande), (Babor, 1, Torpedo), (Estribor, 1, Grande), (Estribor, 1, Torpedo)],
  contenedorSolo ~=? maniobrar superProtegido [(Estribor, 1, Pequeño), (Babor, 1, Grande), (Babor, 1, Torpedo), (Estribor, 1, Grande), (Estribor, 1, Torpedo), (Estribor, 0, Pequeño) ],
  contenedorSolo ~=? maniobrar superProtegido [(Estribor, 0, Torpedo)],
  nave9semiPartida ~=? maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)],
  nave9Partida ~=? maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)]
  ]

testsEj7 = test [
  [] ~=? pruebaDeFuego [peligroFulminanteEstribor] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],
  [] ~=? pruebaDeFuego [peligroFulminanteBabor] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],

  [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9] ~=? pruebaDeFuego [peligroErradoBabor] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],
  [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9] ~=? pruebaDeFuego [peligroErradoEstribor] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],

  [nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],
  [nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],
  [nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],
  [nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9]

  ]

testsEj8 = test [
  1 ~=? componentesPorNivel nave1 0,
  0 ~=? componentesPorNivel nave1 1,
  1 ~=? componentesPorNivel nave2 0,
  2 ~=? componentesPorNivel nave2 1,
  0 ~=? componentesPorNivel nave2 2,
  1 ~=? componentesPorNivel nave4 0,
  2 ~=? componentesPorNivel nave4 1,
  4 ~=? componentesPorNivel nave4 2,
  0 ~=? componentesPorNivel nave4 3,
  2 ~=? componentesPorNivel desbalanceado 2,
  (1, 1) ~=? dimensiones nave1,
  (2, 2) ~=? dimensiones nave2,
  (3, 4) ~=? dimensiones nave4,
  (4, 4) ~=? dimensiones nave6,
  (5, 4) ~=? dimensiones nave8,
  (3, 2) ~=? dimensiones desbalanceado
  ]
