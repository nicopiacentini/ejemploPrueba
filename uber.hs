
--1
data Chofer = UnChofer{
    nombre :: String,
    kilometraje :: Float,
    viajesTomados :: [Viaje],
    condicionDeViaje :: Condicion
}

type Condicion = Viaje -> Bool

data Viaje = UnViaje{
    fecha :: Integer,
    cliente :: Cliente,
    costo :: Float
}

type Cliente = (String,String)
zonaCliente :: Cliente -> String
zonaCliente = snd

nombreCliente :: Cliente -> String
nombreCliente = fst

--2
cualquierViaje :: Condicion
cualquierViaje _ = True

viajeCaro :: Condicion
viajeCaro = (> 200) . costo

nombrePasajeroLargo :: Int -> Condicion
nombrePasajeroLargo cantidad  = (>= cantidad) . length . nombreCliente . cliente

noViveEn :: String -> Condicion
noViveEn zona = (/= zona) . zonaCliente . cliente

--3
luquitas :: Cliente
luquitas = ("Lucas" , "Victoria")

viajeDanielLucas :: Viaje
viajeDanielLucas = UnViaje 20042017 luquitas 150

daniel :: Chofer
daniel = UnChofer "Daniel" 23500 [viajeDanielLucas] (noViveEn "Olilvos")

alejandra :: Chofer
alejandra = UnChofer "alejandra" 180000 [] cualquierViaje

--4
tomaElViaje :: Viaje -> Chofer -> Bool
tomaElViaje viaje  = ($ viaje) . condicionDeViaje

--5
liquidacionChofer :: Chofer -> Float
liquidacionChofer = sum . map costo . viajesTomados


--6
tieneMenosViajes :: Chofer -> Chofer -> Chofer
tieneMenosViajes chofer1 chofer2
    | length (viajesTomados chofer1) > length (viajesTomados chofer2) = chofer1
    | otherwise = chofer2
elDeMenosViajes :: [Chofer] -> Chofer
elDeMenosViajes choferes = foldr (tieneMenosViajes) (head choferes) choferes

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer{viajesTomados = viaje : (viajesTomados chofer)}

realizarUnViaje :: Viaje -> [Chofer] -> Chofer
realizarUnViaje viaje choferes 
    |(length interesados) == 0= agregarViaje viaje . elDeMenosViajes $ interesados
    | otherwise = head choferes
    where interesados = filter (\chofer -> tomaElViaje viaje chofer) choferes

--7
nitoInfy :: Chofer
nitoInfy = UnChofer "Nifo City" 70000 viajesInfinitosLucas (nombrePasajeroLargo 3)

-- No se puede calcular su liquidacion porque la suma
-- de los infitos costos diverge y no es ningun putno en especifico
-- Ahora no puede tampoco tomar el viaje nuevo ya que el nombre "lucas"
-- es una lista de chars de mas de 3 y no cumple con la condicion de nito infy

viajesInfinitos :: Viaje -> [Viaje]
viajesInfinitos viaje = viaje : (viajesInfinitos viaje)

viajesInfinitosLucas :: [Viaje]
viajesInfinitosLucas = viajesInfinitos otroViajeLucas

otroViajeLucas :: Viaje
otroViajeLucas = UnViaje 11032017 luquitas 50

--8
gongNeng :: (Ord a) => a -> (a -> Bool) -> (b -> a) -> [b] -> a
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
