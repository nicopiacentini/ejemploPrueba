data Auto = UnAuto{
    color :: Color,
    velocidad :: Int,
    distancia :: Int
}deriving (Eq , Show)

type Color = String

type Carrera = [Auto]

--1
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 = (> 10) . abs . (distancia auto1 -) . distancia 

leGanaATodos :: Auto -> Carrera -> Bool
leGanaATodos auto = and . map (distancia auto>) . map distancia

leGana :: Auto -> Auto -> Bool
leGana auto1 auto2 = distancia auto1 > distancia auto2

estaTranquilo :: Auto -> Carrera -> Bool
estaTranquilo auto carrera = (leGanaATodos auto carrera &&) . null . filter (estaCerca auto) $ carrera

puesto :: Auto -> Carrera -> Int
puesto auto  = (1 +) . length . filter (leGana auto)

--2
modificarDistancia :: Int -> Auto -> Auto
modificarDistancia distanciaDiferencia auto = auto{distancia = distanciaDiferencia + distancia auto}

correr :: Int -> Auto -> Auto
correr tiempo auto = modificarDistancia (tiempo * velocidad auto) auto


--b
cambiarVelocidad :: Int -> Auto -> Auto
cambiarVelocidad velocidadNueva auto = auto{velocidad = velocidadNueva}

cambiadorVelocidad :: (Int -> Int) -> Auto -> Auto
cambiadorVelocidad cambiador auto = cambiarVelocidad (cambiador . velocidad $ auto) auto

reducirVelocidad :: Int -> Int -> Int
reducirVelocidad reduccion velocidad = max 0 (velocidad - reduccion)

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad reduccion auto = cambiadorVelocidad (reducirVelocidad reduccion) auto


afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

--3
type PowerUp = Auto -> Carrera -> Carrera
terremoto :: PowerUp
terremoto auto  = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) 

miguelitos :: Int -> PowerUp
miguelitos intensidad auto = afectarALosQueCumplen (leGana auto) (bajarVelocidad intensidad)

jetPack :: Int -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (==auto) (correr (2* tiempo)) carrera

posicionYColor :: [Auto] -> [Auto] -> [(Int,Color)]
posicionYColor [] _ = []
posicionYColor (auto : autos) carreraFinal = (puesto auto carreraFinal , color auto) : posicionYColor autos carreraFinal

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera carrera powerups = posicionYColor carreraFinal carreraFinal
    where carreraFinal = foldr ($) carrera $ powerups


--b
correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo = map (correr tiempo)

esDeColor :: Color -> Auto -> Bool
esDeColor colorElegido = (== colorElegido) . color

autoDeColor :: Color -> Carrera -> Auto
autoDeColor colorElegido carrera = head . filter (esDeColor colorElegido) $ carrera

usaPowerUp :: Color -> PowerUp -> Carrera -> Carrera
usaPowerUp colorElegido powerup carrera = powerup (autoDeColor colorElegido carrera) carrera



--c
autoRojo :: Auto
autoRojo = UnAuto "rojo" 120 0

autoAzul :: Auto
autoAzul = UnAuto "azul" 120 0

autoBlanco :: Auto
autoBlanco = UnAuto "blanco" 120 0

autoNegro :: Auto
autoNegro = UnAuto "negro" 120 0

carrerita :: Carrera
carrerita = [autoAzul , autoBlanco , autoNegro , autoRojo]


eventos :: [Carrera -> Carrera]
eventos = [correnTodos 30 , jetPack 3 autoAzul , terremoto autoBlanco , correnTodos 40 , miguelitos 20 autoBlanco , jetPack 6 autoNegro , correnTodos 10]
