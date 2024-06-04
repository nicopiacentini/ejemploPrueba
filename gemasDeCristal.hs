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

--modificarSituacion :: Situacion -> (Float -> Float) -> Situacion
--modificarSituacion situacion funcion = map ()