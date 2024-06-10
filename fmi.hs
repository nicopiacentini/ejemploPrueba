--1

data Pais = UnPais{
    nombre :: String,
    ingresoPerCapita :: Float,
    activaSectorPublico :: Int,
    activaSectorPrivado :: Int,
    recursosNaturales :: [String],
    deudaActual :: Float -- en millones de dolares
} deriving Show

namibia :: Pais
namibia = UnPais "Namibia" 4140 400000 650000 ["mineria" , "ecoculturismo"] 50

--2
type Estrategia = Pais -> Pais
modificarDeuda :: Float -> Pais -> Pais
modificarDeuda modificacion pais = pais{deudaActual = modificacion + deudaActual pais}
prestarNMillones :: Float -> Estrategia
prestarNMillones cantidad = modificarDeuda (1.5 * cantidad)

modificarIPC :: Float -> Pais -> Pais
modificarIPC modificacion pais = pais{ingresoPerCapita = modificacion + ingresoPerCapita pais}

modificarSectorPublico :: Int -> Pais -> Pais
modificarSectorPublico modificacion pais = pais{activaSectorPublico = max 0 (modificacion + activaSectorPublico pais)}

reducirXPuestosPublicos :: Int -> Estrategia
reducirXPuestosPublicos cantidad pais
    | cantidad > 100 = modificarSectorPublico (-cantidad) . modificarIPC (-0.2 * ingresoPerCapita pais) $ pais
    | otherwise = modificarSectorPublico (- cantidad) . modificarIPC (-0.15 * ingresoPerCapita pais) $ pais

quitarRecurso :: String -> Pais -> Pais
quitarRecurso recurso pais = pais{recursosNaturales = filter (/= recurso) (recursosNaturales pais)}
entregarRecurso :: String -> Estrategia
entregarRecurso recurso = modificarDeuda (-2) . quitarRecurso recurso

pbi :: Pais -> Float
pbi pais = (ingresoPerCapita pais) * fromIntegral(activaSectorPrivado pais + activaSectorPublico pais) 

establecerBlindaje :: Estrategia
establecerBlindaje pais = prestarNMillones (pbi pais) . modificarSectorPublico (-500) $ pais


--3
type Receta = [Estrategia]

unaReceta :: Receta
unaReceta = [prestarNMillones 200 , entregarRecurso "mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta

--4

tieneRecurso :: String -> Pais -> Bool
tieneRecurso recurso = any (== recurso) . recursosNaturales

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (tieneRecurso "petroleo")

deudaTotal :: [Pais] -> Float
deudaTotal = sum . map (deudaActual)

--5
estaOrdenada :: (Num a , Ord a) => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (unNumero : otroNumero : numeros) = unNumero > otroNumero && estaOrdenada (otroNumero : numeros) 

sonRecetasOrdenadasDeMenorAMayor :: [Receta] -> Pais -> Bool
sonRecetasOrdenadasDeMenorAMayor recetas pais = estaOrdenada . map pbi . map (flip aplicarReceta pais) $ recetas

--6
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

paisRandom :: Pais
paisRandom = namibia{recursosNaturales = recursosNaturalesInfinitos}

-- cuando se aplique la funcion puedezafar, el programa entra en un bucle infinito por la lazy evaluation.
-- esto implica que el programa va a buscar hasta encontrar "petroleo" o hasta terminar la lista.
-- como no ocurre nada de esto, entra en un bucle infinito
-- la funcion de deuda total si es aplicable porque no mira los recursos naturales, solamente la deuda de cada pais con el fmi  