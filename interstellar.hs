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
pasarTiempo' :: Astronauta -> Int -> Astronauta
pasarTiempo' astronauta anios = modificarEdad (edadAstronauta astronauta + (comoPasaElTiempo(planetaActual astronauta))anios) astronauta

tiempoViaje :: Astronauta -> Astronauta -> Nave -> Int
tiempoViaje astronauta1 astronauta2 nave = nave (planetaActual astronauta1) (planetaActual astronauta2)
pasarTiempoEnOtroPlaneta :: Int -> Astronauta -> Astronauta
pasarTiempoEnOtroPlaneta aniosTerrestres astronauta = pasarTiempo' astronauta . comoPasaElTiempo (planetaActual astronauta) $ aniosTerrestres
rescate :: [Astronauta] -> Astronauta -> Nave -> [Astronauta]
rescate astronautas astronautaVarado nave = map (pasarTiempo tiempoDeViaje) . ((pasarTiempoEnOtroPlaneta tiempoDeViaje astronautaVarado) :) . map (pasarTiempo tiempoDeViaje) $ astronautas
    where tiempoDeViaje = tiempoViaje (head astronautas) astronautaVarado nave

esViejo :: Astronauta -> Bool
esViejo = (> 90) . edadAstronauta

puedeSerRescatado :: [Astronauta] -> Nave -> Astronauta -> Bool
puedeSerRescatado astronautas nave varado = all (< 90) . map edadAstronauta $ rescate astronautas varado nave

puedenSerRescatados :: [Astronauta] -> Nave -> [Astronauta] -> [String]
puedenSerRescatados astronautas nave varados = map nombreAstronauta . filter (puedeSerRescatado astronautas nave) $ varados

