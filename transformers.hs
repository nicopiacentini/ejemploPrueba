import Text.Show.Functions()
data Autobot = Robot{
    nombreRobot :: String,
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
    nombre = nombreRobot autobot,
    caracteristicasAuto = (transformacionAauto autobot) (habilidades autobot)
}

--4
velocidadContra :: Autobot -> Autobot -> Int
velocidadContra autobot1 autobot2 = (velocidadAutobot autobot1) - (max 0 (fuerzaAutobot autobot2 - resistenciaAutobot autobot1))