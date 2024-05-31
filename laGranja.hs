data Animal = UnAnimal{
    nombre :: String,
    edad :: Int,
    peso :: Float,
    tipo :: String,
    estaEnfermo :: Bool
}
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
type Actividad = Animal -> Animal