module Lib where

import Text.Show.Functions

data Mago = Mago{
    nombre :: String,
    edad :: Int,
    salud :: Int,
    hechizos :: [Hechizo]
}deriving (Show) 

type Hechizo = Mago -> Mago

instance Eq Mago where
    (Mago nombre1 edad1 salud1 _) == (Mago nombre2 edad2 salud2 _) =
        nombre1 == nombre2 && edad1 == edad2 && salud1 == salud2

lagrimaFenix :: Int -> Hechizo
lagrimaFenix curacion mago = mago{salud = salud mago + curacion}

sectumSempra :: Hechizo
sectumSempra mago = mago{salud = salud mago - efectoSectum (salud mago)}

efectoSectum :: Int -> Int
efectoSectum salud | salud > 10 = 10
                   | otherwise  = div salud 2

obliviate :: Int -> Hechizo
obliviate olvidados mago = mago{hechizos = drop olvidados (hechizos mago)}

confundus :: Hechizo
confundus mago = primerHechizo mago
            where primerHechizo = head (hechizos mago)

poder :: Mago -> Int
poder mago = salud mago + (edad mago * length (hechizos mago))

daño :: Mago -> Hechizo -> Int
daño mago hechizo =  salud mago - salud(hechizo mago)

diferenciaDePoder :: Mago -> Mago -> Int
diferenciaDePoder mago otroMago = abs (poder mago - poder otroMago)

type AcademiaMagica = [Mago]

hogwarts :: AcademiaMagica
hogwarts = [potter, weasley, malfoy, goyle, granger, snape, dumbledore, crabbe, rubeus]

koldovstoretz :: AcademiaMagica
koldovstoretz = [bazarov, ibrahimova]

beauxbatons :: AcademiaMagica
beauxbatons = []

magoSinHechizos :: String -> AcademiaMagica -> Bool
magoSinHechizos nombreMago = any (\mago -> nombre mago == nombreMago &&  length (hechizos mago) == 0)

magosViejos :: AcademiaMagica -> AcademiaMagica
magosViejos = filter ((> 16) . edad)

esNionio :: Mago -> Bool
esNionio mago = length (hechizos mago) > 3 * edad mago

viejosNionios :: AcademiaMagica -> Bool
viejosNionios[] = True
viejosNionios academiaMagica  = all esNionio (magosViejos academiaMagica)

{-
    Analizar la siguiente funcion:

    f x [y] = y
    f x (y1:y2:ys)
        | x y1 >= x y2 = f x (y1:ys)
        | otherwise = f x (y2 : ys)

    Se trata de una funcion que recibe una lista y devuelve su maximo.
    En el caso base con un solo elemento devuelve dicho elemento.
    Si no va comparando los 2 primeros elementos de la lista,descarta el menor y de manera recursiva
    recorre la lista hasta el final.

    Esta funcion nos sirve a la hora de tener que utilizar el maximo de una lista para alguna aplicacion(como las funciones de este punto)

 -}

---Mejoro expresividad
obtenerMaximo :: Ord a1 => (a2 -> a1) -> [a2] -> a2
obtenerMaximo hechizos [elemento] = elemento
obtenerMaximo hechizos (primerElemento:segundoElemento:cola)| hechizos primerElemento >= hechizos segundoElemento = obtenerMaximo hechizos (primerElemento:cola)
                                                         | otherwise = obtenerMaximo hechizos (segundoElemento : cola)

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra objetivo agresor = obtenerMaximo (\hechizo -> daño objetivo hechizo) (hechizos agresor)

mejorOponente :: Mago -> AcademiaMagica -> Mago
mejorOponente mago academia = mago


potter = Mago{nombre = "Harry", edad = 20, salud = 100, hechizos = [lagrimaFenix 20]}
weasley = Mago{nombre = "Ron", edad = 21, salud = 5, hechizos = [lagrimaFenix 30, sectumSempra ]}
malfoy = Mago{nombre = "Draco", edad = 22, salud = 10, hechizos = [lagrimaFenix 10, sectumSempra]}
goyle = Mago{nombre = "Gregory", edad = 20, salud = 2, hechizos = []}
granger = Mago{nombre = "Hermione", edad = 21, salud = 70, hechizos = [lagrimaFenix 10, sectumSempra, obliviate 1]}
snape = Mago{nombre = "Severus", edad = 50, salud = 200, hechizos = [sectumSempra, obliviate 2, lagrimaFenix 10]}
dumbledore = Mago{nombre = "Albus", edad = 50, salud = 350, hechizos = [obliviate 2, lagrimaFenix 10, sectumSempra]}
crabbe = Mago{nombre = "Vincent", edad = 9, salud = 6, hechizos = []}
rubeus = Mago{nombre = "Hagrid", edad = 40, salud = 200, hechizos = []}
bazarov = Mago{nombre = "Spartacus", edad = 45, salud = 120, hechizos = [lagrimaFenix 10, sectumSempra]}
ibrahimova = Mago{nombre = "Stella", edad = 50, salud = 140, hechizos = [lagrimaFenix 10, sectumSempra]}