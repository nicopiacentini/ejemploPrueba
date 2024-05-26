type Persona = (Int , Float , Float)
pancho :: Persona
pancho = ( 40, 120, 1)
andres :: Persona
andres = ( 22, 80, 6)

type Ejercicio = Persona -> Int -> Persona

relax :: Ejercicio
relax persona _ = persona
tonificacion :: Persona -> Float
tonificacion (_ , _ , toni) = toni

peso :: Persona -> Float
peso (_ , p , _) = p

edad :: Persona -> Int
edad (e, _ , _) = e

esObeso :: Persona -> Bool
esObeso persona = (>100) . peso $ persona

esTonificado :: Persona -> Bool
esTonificado = (>5) . tonificacion

--1
esSaludable :: Persona -> Bool
esSaludable persona = (esTonificado persona &&) . not . esObeso $ persona


--2
kiloPorCalorias :: Float -> Float
kiloPorCalorias calorias = calorias / 150.0

modificarPeso :: Persona -> Float -> Persona
modificarPeso (age , weight , tonification) modificacion = (age , weight + modificacion , tonification)

bajarDePeso :: Persona -> Float -> Persona
bajarDePeso persona calorias
    | esObeso persona = modificarPeso persona (0.0 - (kiloPorCalorias calorias))
    | ((> 30) . edad $ persona)  && (calorias > 200) = modificarPeso persona (0 - 1)
    | otherwise = modificarPeso persona (0 - (calorias / ((peso persona) * fromIntegral(edad persona))))


--3
velocidadesEntrenamiento :: Int -> [Float] -> [Float]
velocidadesEntrenamiento minutos velocidades
    | minutos > 5 = velocidadesEntrenamiento (minutos - 5) (((last velocidades) + 1) : velocidades)
    | otherwise   = velocidades

velocidadPromedioEntrenamiento :: Int -> Float
velocidadPromedioEntrenamiento minutos = (sum (velocidadesEntrenamiento minutos [6.0]))  / (fromIntegral ( 1 + div minutos 5))

caminata :: Int -> Persona -> Persona
caminata minutos persona = bajarDePeso persona  (fromIntegral (minutos * 5))

entrenamientoEnCinta :: Persona -> Int -> Persona
entrenamientoEnCinta persona minutos = bajarDePeso persona ((fromIntegral minutos) * (velocidadPromedioEntrenamiento minutos))


--b
modificarTonificacion :: Float -> Persona -> Persona
modificarTonificacion modificacion (age , weight , toni)  = (age , weight , toni + modificacion)
pesas :: Int -> Float -> Persona -> Persona
pesas minutos mancuerna persona
    | minutos > 10 = modificarTonificacion  (mancuerna / 10) persona
    | otherwise = persona

--c 2 calorias por minuto por inclinacion
colina :: Float -> Float -> Persona -> Persona
colina minutos inclinacion persona= bajarDePeso persona (2 * inclinacion *  minutos)

--d
montania :: Float -> Float -> Persona -> Persona
montania minutos inclinacionInicial persona = modificarTonificacion 1 . colina (minutos / 2) (3 * inclinacionInicial) . colina (minutos / 2) inclinacionInicial $ persona


--4

--a
type Rutina = (String , Int , [Int -> Persona -> Persona])
ejercicios :: Rutina -> [Int -> Persona -> Persona]
ejercicios (_ , _ , exercises) = exercises
tiempo :: Rutina -> Int
tiempo (_ , t , _) = t

rutinaDeEjercicios :: Rutina -> Persona -> Persona
rutinaDeEjercicios rutina persona = foldl (\personita ejercicio -> ejercicio (tiempo rutina) personita) persona (ejercicios rutina)

rutinaEjerciciosRecursiva :: Rutina -> Persona -> Persona
rutinaEjerciciosRecursiva (_ , _ , []) persona = persona
rutinaEjerciciosRecursiva (nombre , time , (ejercicio : otrosEjercicios)) persona = rutinaEjerciciosRecursiva (nombre , time , otrosEjercicios) (ejercicio time persona)

--5
esSaludableConRutina :: Persona -> Rutina -> Bool
esSaludableConRutina persona rutina = esSaludable . rutinaDeEjercicios rutina $ persona

loHacenSaludable :: [Rutina] -> Persona -> [Rutina]
loHacenSaludable rutinas persona = filter (esSaludableConRutina persona) rutinas