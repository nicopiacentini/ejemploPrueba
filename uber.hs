
--1
data Chofer = UnChofer{
    nombre :: String,
    kilometraje :: Float,
    viajesTomados :: [Viaje],
    condicionDeViaje :: Condicion
}

type Condicion = Viaje -> Bool

data Viaje = UnViaje{
    fecha :: Int,
    cliente :: Cliente,
    costo :: Float
}

type Cliente = (String,String)
direccion :: Cliente -> String
direccion = snd

nombreCliente :: Cliente -> String
nombreCliente = fst

--2
cualquierViaje :: Condicion
cualquierViaje _ = True

viajeCaro :: Condicion
viajeCaro = (> 200) . costo

nombrePasajeroLargo :: Int -> Condicion
nombrePasajeroLargo cantidad  = (> cantidad) . length . nombreCliente . cliente

noViveEn :: String -> Condicion
noViveEn zona = (/= zona) . direccion . cliente