--1
data Animal = UnAnimal{
    coeficienteIntelectual :: Int,
    especie :: String,
    capacidades :: [String]
}deriving Show


type Transformacion = Animal -> Animal

modificarCI :: Int -> Transformacion
modificarCI numero animal = animal{coeficienteIntelectual = numero + coeficienteIntelectual animal}
modificarHabilidades :: [String] -> Transformacion
modificarHabilidades [] animal = animal{capacidades = [] }
modificarHabilidades habilidades animal = animal{capacidades = habilidades ++ capacidades animal}

inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior incremento animal = modificarCI incremento animal

pinkificar :: Transformacion
pinkificar = modificarHabilidades []  

elefante :: Animal
elefante = UnAnimal 100 "mamifero" ["Stomping" , "big ass trompa"]
raton :: Animal
raton = UnAnimal 150 "mamifero" ["comer queso" , "vision nocturna"]

animalInteligente :: Animal -> Bool
animalInteligente animal = (>100) . coeficienteIntelectual $ animal

superPoderes :: Transformacion
superPoderes elefante = modificarHabilidades ("no tenerle miedo a ratones" : []) elefante
superPoderes raton
    |animalInteligente raton = modificarHabilidades ("hablar" : []) raton
    |otherwise = raton

superPoderes animalGenerico = animalGenerico

--3
coeficienteMayorA :: Int -> Animal -> Bool
coeficienteMayorA coeficiente animal = (>coeficiente) . coeficienteIntelectual $ animal

puedeHablar :: Animal -> Bool
puedeHablar = elem "hablar" . capacidades

type Criterio = Animal -> Bool
antropomorfico :: Criterio
antropomorfico animal = coeficienteMayorA 60 animal && puedeHablar animal


noTanCuerdo :: Criterio
noTanCuerdo = (> 2) . length . filter pinkiesco . capacidades

pinkiesco :: String -> Bool
pinkiesco habilidad = (empiezaConHacer habilidad &&) . (>= 10) . length $ habilidad

empiezaConHacer :: String -> Bool
empiezaConHacer habilidad = (== "hacer") . take 5 $ habilidad


--4
type Experimento = ([Transformacion] , Criterio)
transformaciones :: Experimento -> [Transformacion]
transformaciones = fst

criterioExperimento :: Experimento -> Criterio
criterioExperimento = snd

realizarExperimentos :: Animal -> Experimento -> Animal
realizarExperimentos animal experimento = foldl (\animalito transformacion -> transformacion animalito) animal . transformaciones $ experimento

experimentoExitoso :: Animal -> Experimento -> Bool
experimentoExitoso animal experimento = (criterioExperimento experimento) . realizarExperimentos animal $ experimento

--5
tieneAlgunaCapacidad :: [String] -> Animal -> Bool
tieneAlgunaCapacidad [] _ = False
tieneAlgunaCapacidad (capacidad : capacities) animal = any (==capacidad) (capacidades animal) || tieneAlgunaCapacidad capacities animal 


informeCI :: [Animal] -> [String] -> Experimento -> [Int]
informeCI animales capacidades experimento = map coeficienteIntelectual . filter (\animaloso -> tieneAlgunaCapacidad capacidades animaloso). map (\animal -> realizarExperimentos animal experimento)  $ animales

tieneTodasLasCapacidades :: [String] -> Animal -> Bool
tieneTodasLasCapacidades [] _ = True
tieneTodasLasCapacidades (capacidad : capacities) animal = any (==capacidad) (capacidades animal) && tieneAlgunaCapacidad capacities animal 

informeEspecie :: [Animal] -> [String] -> Experimento -> [String]
informeEspecie animales capacidades experimento = map especie . filter (\animaloso -> tieneTodasLasCapacidades capacidades animaloso). map (\animal -> realizarExperimentos animal experimento)  $ animales

noTieneAlgunaCapacidad :: [String] -> Animal -> Bool
noTieneAlgunaCapacidad [] _ = True
noTieneAlgunaCapacidad (capacidad : capacities) animal = all (/=capacidad) (capacidades animal) && tieneAlgunaCapacidad capacities animal 

--informeCantidadCapacidades :: [Animal] -> [String] -> Experimento -> [Int]
--informeCantidadCapacidades animales capacidades experimento = 
--    map (length . capacidades) . filter (\animal -> noTieneAlgunaCapacidad capacidades animal) . map (\animal -> realizarExperimentos animal experimento) $ animales

--7
generateWordsUpTo :: Int -> [String]
generateWordsUpTo numero = (show numero) : []

todasPalabrasPinkiescas :: [String]
todasPalabrasPinkiescas = map ("hacer " ++) (generateWordsUpTo 4)