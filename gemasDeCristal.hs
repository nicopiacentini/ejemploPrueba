data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)
type Situacion = [Aspecto]

mejorAspecto mejor peor = grado mejor < grado peor
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)
--1
modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto funcion aspecto = aspecto{grado = funcion (grado aspecto)}

situacionMejor :: Situacion -> Situacion -> Bool
situacionMejor mejor peor = and . map (\aspecto -> mejorAspecto aspecto (buscarAspecto aspecto peor)) $ mejor

modificarGrado :: Aspecto -> Float -> Aspecto
modificarGrado aspecto nuevoGrado = aspecto{grado = nuevoGrado}

modificarSituacion :: (Float -> Float) -> Aspecto -> Situacion -> Situacion
modificarSituacion _ _ []= []
modificarSituacion funcion aspecto' (aspecto : aspectos)
    | aspecto == aspecto' = (modificarAspecto (funcion) aspecto) : aspectos
    | otherwise = aspecto : modificarSituacion funcion aspecto' aspectos

--2
data Gema = UnaGema{
    nombreGema :: String,
    fuerza :: Float,
    personalidad :: Personalidad
}
incertidumbre :: Aspecto
incertidumbre = UnAspecto "incertidumbre" 100
tension :: Aspecto
tension = UnAspecto "tension" 120
type Personalidad = Situacion -> Situacion
vidente :: Personalidad
vidente = modificarSituacion (/2) incertidumbre . modificarSituacion (0 - 10 +) tension

--3
leGana :: Gema -> Gema -> Situacion -> Bool
leGana gemaGanadora gemaPerdedora situacion = 
    (fuerza gemaGanadora > fuerza gemaPerdedora) && (situacionMejor ((personalidad gemaGanadora) situacion) ((personalidad gemaPerdedora) situacion))

--4
modificarGradoSituacion :: Float -> Situacion -> Situacion
modificarGradoSituacion diferencia situacion = map (\aspecto -> modificarAspecto (diferencia +) aspecto) situacion
nombreFusion :: Gema -> Gema -> String
nombreFusion gema1 gema2
    | nombreGema gema1 == nombreGema gema2 = nombreGema gema1
    | otherwise = nombreGema gema1 ++ nombreGema gema2

sonCompatibles :: Gema -> Gema -> Situacion -> Bool
sonCompatibles gema1 gema2 situacion =  
    situacionMejor ((personalidad gema1 . personalidad gema2 $ situacion)) ((personalidad gema1) situacion) && situacionMejor ((personalidad gema1 . personalidad gema2 $ situacion)) ((personalidad gema2) situacion)

fuerzaFusion :: Gema -> Gema -> Situacion -> Float
fuerzaFusion gema1 gema2 situacion
    |sonCompatibles gema1 gema2 situacion = (fuerza gema1 + fuerza gema2) * 10
    | leGana gema1 gema2 situacion = 7 * fuerza gema1
    | otherwise = 7 * fuerza gema2


fusion :: Situacion -> Gema -> Gema ->  Gema
fusion situacion gema1 gema2 = UnaGema{
    nombreGema = nombreFusion gema1 gema2,
    fuerza = fuerzaFusion gema1 gema2 situacion,
    personalidad = personalidad gema1 . personalidad gema2 . modificarGradoSituacion (-10)
}

--5
fusionGrupal :: [Gema] -> Situacion -> Gema
fusionGrupal gemas situacion= foldl (fusion situacion) (head gemas) (drop 1 gemas)