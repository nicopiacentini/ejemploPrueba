import Text.Show.Functions()
data Autobot = Robot{
    nombre :: String,
    habilidades :: (Int , Int , Int),
    transformacionAauto :: Transformacion
} | Vehiculo{
    nombre :: String,
    caracteristicasAuto :: (Int, Int)
}deriving (Show)

type Transformacion = (Int , Int , Int) -> (Int , Int) 

optimus :: Autobot
optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion :: Transformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz :: Autobot
jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion :: Transformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack :: Autobot
wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion :: Transformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee :: Autobot
bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion :: Transformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots :: [Autobot]
autobots = [ optimus, jazz, wheeljack, bumblebee ]

--1
maximoSegun :: (Ord a) => (a -> a -> a) -> a -> a -> a
maximoSegun f valor1 valor2
    | f valor1 valor2 > f valor2 valor1 = valor1
    | otherwise = valor2

--2
fuerzaAutobot :: Autobot -> Int
fuerzaAutobot (Robot _ (fuerza ,_,_) _) = fuerza
fuerzaAutobot (Vehiculo _ _ ) = 0

velocidadAutobot :: Autobot -> Int
velocidadAutobot (Robot _ (_,velocidad,_) _) = velocidad
velocidadAutobot (Vehiculo _ (velocidad,_)) = velocidad

resistenciaAutobot :: Autobot -> Int
resistenciaAutobot (Robot _ (_,_,resistencia) _) = resistencia
resistenciaAutobot (Vehiculo _ (_,resistencia) ) = resistencia

--3
transformacion :: Autobot -> Autobot
transformacion autobot = Vehiculo{
    nombre = nombre autobot,
    caracteristicasAuto = (transformacionAauto autobot) (habilidades autobot)
}

--4
velocidadContra :: Autobot -> Autobot -> Int
velocidadContra autobot1 autobot2 = (velocidadAutobot autobot1) - (max 0 (fuerzaAutobot autobot2 - resistenciaAutobot autobot1))

--5
elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido autobot1 autobot2
    | (velocidadContra autobot1 autobot2) > (velocidadContra autobot2 autobot1) = autobot1
    | otherwise = autobot2

--6
velocidades :: Autobot -> (Int , Int)
velocidades autobot = (velocidadAutobot autobot , velocidadAutobot . transformacion $ autobot)

tuplaMayor :: (Int , Int) -> (Int , Int) -> Bool
tuplaMayor (v1,v2) (v3,v4) = (v1>v3) && (v1>v4) && (v2>v3) && (v2>v4)


domina :: Autobot -> Autobot -> Bool
domina autobot1 autobot2 = tuplaMayor (velocidades autobot1) . velocidades $ autobot2

--b
losDomaATodos :: Autobot -> [Autobot] -> Bool
losDomaATodos autobot = and . map (domina autobot) 


--7
quienesCumplen :: (Autobot -> Bool) -> [Autobot] -> [String]
quienesCumplen criterio  = map nombre . filter criterio

--b
losDominaATodos :: Autobot -> [Autobot] -> Bool
losDominaATodos autobot botsos = (== length botsos) . length . quienesCumplen ((=='a') . last . nombre) . filter (domina autobot) $ botsos

--8
saraza :: (Ord x) => x -> x -> x -> (x -> x -> x) -> x
saraza x y w z = z w . maximoSegun z y $ x