data Animal = UnAnimal{
    nombre :: String,
    edad :: Int,
    peso :: Float,
    tipo :: String,
    estaEnfermo :: Bool
}deriving Show
type DiasYCosto = (Int , Float)
type VisitaMedica = Animal -> DiasYCosto
dias :: DiasYCosto  -> Int
dias = fst  

--1
tieneNombreFalopa :: Animal -> Bool
tieneNombreFalopa animal = (("sagitario" == tipo animal) &&) . (=='i') . last . nombre $ animal 

laPasoMal :: VisitaMedica -> Animal -> Bool
laPasoMal visita = (> 30) . dias . visita

--2
vacaJuan :: Animal
vacaJuan = UnAnimal "VacaJuan" 10 690 "pacogonzales" False
modificarPeso :: Float -> Animal -> Animal
modificarPeso nuevoPeso animal = animal{peso = nuevoPeso}
type Actividad = Animal -> Animal
engorde :: Float -> Actividad
engorde kilos animal = modificarPeso ( (min 5 (kilos / 2)) + peso animal ) animal
--b
revisacion :: Actividad
revisacion animal
    | estaEnfermo animal = engorde 2 animal
    | otherwise = animal

modificarEdad :: Int -> Animal -> Animal
modificarEdad edadNueva animal = animal{edad = edadNueva}
--c
festejoPumple :: Actividad
festejoPumple animal = modificarPeso ( -1 + peso animal) . modificarEdad ( 1 + edad animal) $ animal
--d
enfermar :: Animal -> Animal
enfermar animal = animal{estaEnfermo = True}
chequeoPeso :: Float -> Actividad
chequeoPeso pesoMinimo animal
    | pesoMinimo < peso animal = animal
    | otherwise = enfermar animal

--3

proceso :: Animal -> [Actividad] -> Animal
proceso animal actividades = foldr ($) animal actividades

--4
diferenciaPeso :: Animal -> Animal -> Float
diferenciaPeso antes despues = (peso despues) - (peso antes)
diferenciaPesoAceptable :: Animal -> Animal -> Bool
diferenciaPesoAceptable animalAntes animalDespues = (diferenciaPeso animalAntes animalDespues) >= 0 && ((diferenciaPeso animalAntes animalDespues)) < 3

mejora :: [Actividad] -> Animal -> Bool
mejora [] _ = True
mejora (actividad : actividades) animal
    | diferenciaPesoAceptable animal . actividad $ animal = mejora actividades animal
    | otherwise = False

--5
primeros3falopa :: [Animal] -> [Animal]
primeros3falopa = take 3 . filter tieneNombreFalopa

--en mi caso no porque mi funcion primeros3Falopa primero filtra y luego toma los primeros 3, sin embargo, nunca termina de filtrar