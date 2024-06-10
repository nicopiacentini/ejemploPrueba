--1

data Pais = UnPais{
    nombre :: String,
    ingresoPerCapita :: Float,
    activaSectorPublico :: Int,
    activaSectorPrivado :: Int,
    recursosNaturales :: [String],
    deudaActual :: Float -- en millones de dolares
}

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



