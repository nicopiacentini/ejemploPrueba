type Posicion = (Float , Float , Float)
data Planeta = UnPlaneta{
    nombrePlaneta :: String,
    posicionPlaneta :: Posicion,
    comoPasaElTiempo :: Int -> Int
}
type Coordenada = Posicion -> Float
coordX :: Coordenada
coordX (x,_,_) = x
coordY :: Coordenada
coordY (_,y,_) = y
coordZ :: Coordenada
coordZ (_,_,z) = z

data Astronauta = UnAstronauta{
    nombreAstronauta :: String,
    edadAstronauta :: Int,
    planetaActual :: Planeta
}

--1
distancia :: Planeta -> Planeta -> Float
distancia planeta1 planeta2 = sqrt((coordX (posicionPlaneta planeta1) - coordX (posicionPlaneta planeta2)) ^ 2 + (coordY (posicionPlaneta planeta1) - coordY (posicionPlaneta planeta2)) ^ 2 + (coordZ (posicionPlaneta planeta1) - coordZ (posicionPlaneta planeta2)) ^ 2)
tiempo :: Int -> Planeta -> Planeta -> Int
tiempo velocidad planeta1 planeta2 = div (round(distancia planeta1 planeta2)) velocidad

--2
modificarEdad :: Int -> Astronauta -> Astronauta
modificarEdad edadNueva astronauta = astronauta{edadAstronauta = edadNueva}

pasarTiempo :: Int -> Astronauta -> Astronauta
pasarTiempo anios astronauta = modificarEdad (edadAstronauta astronauta + (comoPasaElTiempo(planetaActual astronauta))anios) astronauta

--3
type Nave = Planeta -> Planeta -> Int
naveVieja :: Int -> Nave
naveVieja tanquesDeOxigeno planeta1 planeta2
    | tanquesDeOxigeno >= 6 = tiempo 7 planeta1 planeta2
    | otherwise = tiempo 10 planeta1 planeta2

naveNueva :: Nave
naveNueva _ _ = 0

viaje :: Astronauta -> Planeta -> Nave -> Astronauta
viaje astronauta planetaNuevo nave = pasarTiempo (nave (planetaActual astronauta) planetaNuevo) astronauta

--4


