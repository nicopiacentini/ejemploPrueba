
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
nombrePasajeroLargo cantidad  = (> cantidad) . length . nombreCliente . cliente

noViveEn :: String -> Condicion
noViveEn zona = (/= zona) . zonaCliente . cliente

--3
luquitas :: Cliente
luquitas = ("Lucas" , "Victoria")

viajeDanielLucas :: Viaje
viajeDanielLucas = UnViaje 20042017 luquitas 150

daniel :: Chofer
daniel = UnChofer "Daniel" 23500 [viajeDanielLucas] (noViveEn "Olilvos")