data  Persona = UnaPersona{
    habilidadesPersona :: [String],
    sonBuenas :: Bool
}
juan :: Persona
juan = UnaPersona ["volar" , "comer"] True

type Color = String
data PowerRanger = UnPowerRanger{
    color :: Color,
    habilidadesPower :: [String],
    nivelDePelea :: Int
} deriving Show

powerRojo :: PowerRanger
powerRojo = UnPowerRanger "rojo" ["volar" , "correr"] 200

superHabilidad :: String -> String
superHabilidad palabra = "Super " ++ palabra

poderPower :: Persona -> Int
poderPower persona = sum . map length . habilidadesPersona $ persona

convertirEnPower :: Persona -> Color -> PowerRanger
convertirEnPower persona color = UnPowerRanger color (map superHabilidad (habilidadesPersona persona)) (poderPower persona)

--3
jason :: Persona
jason = UnaPersona ["volar" , "queso" , "nigger"] True
kimberly :: Persona
kimberly = UnaPersona ["sape" ,"balls"] False
skull :: Persona
skull = UnaPersona ["balls" ,"niga"] False
formarEquipoRanger :: [Color] -> [Persona] -> [PowerRanger]
formarEquipoRanger _ [] = []
formarEquipoRanger [] _ = []
formarEquipoRanger colores (persona : personas) 
    | sonBuenas persona =  (convertirEnPower persona (head colores)) : formarEquipoRanger (drop 1 colores) personas
    | otherwise = formarEquipoRanger colores personas

--4 a
type Condicion a = a -> Bool
findOrElse :: Condicion a -> a -> [a] -> a
findOrElse _ valor [] = valor
findOrElse condicion valor (value : values) 
    | condicion value = value
    | otherwise = findOrElse condicion valor values

--b
rangerLider :: [PowerRanger] -> PowerRanger
rangerLider rangers = findOrElse esRojo (head rangers) rangers

esRojo :: PowerRanger -> Bool
esRojo = (=="rojo") . color

--5
maximo :: (Ord b) => (a -> b) -> a -> a -> a
maximo f elemento1 elemento2
    | (f elemento1) > (f elemento2) = elemento1
    | otherwise = elemento2
maximumBy :: (Ord b) => [a] -> (a -> b) -> a
maximumBy elementos f = foldl (\elemento1 elemento2 -> maximo (f) elemento1 elemento2) (head elementos) elementos

--b
rangerMasPoderoso :: [PowerRanger] -> PowerRanger
rangerMasPoderoso rangers = maximumBy rangers nivelDePelea


--6
rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso powerRanger = (> 5) . length . habilidadesPower $ powerRanger

--7
--a
alfa5 :: PowerRanger
alfa5 = UnPowerRanger "metalico" ("reparar cosas" : (repeat "ay")) 0
--b
-- no se puede rangerHabilidoso
data ChicaSuperpoderosa = UnaChicaSuperpoderosa{
    colorChica :: Color,
    cantidadDePelo :: Int
}

chicaLider :: [ChicaSuperpoderosa] -> ChicaSuperpoderosa
chicaLider chicas = findOrElse esRoja (head chicas) chicas

esRoja :: ChicaSuperpoderosa -> Bool
esRoja chica = (=="rojo") . colorChica $ chica

