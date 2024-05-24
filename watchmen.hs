type Vigilante = (String , [String] , Int)
algunosVigilantes :: [Vigilante]
algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962), ("Ozimandias", ["Inteligencia", "Más Inteligencia Aún"], 1968), ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)]

type Evento = [Vigilante] -> [Vigilante]

nombreVigilante :: Vigilante -> String
nombreVigilante (nombre , _ , _) = nombre

habilidadesVigilante :: Vigilante -> [String]
habilidadesVigilante (_ , habilidades , _) = habilidades

anioVigilante :: Vigilante -> Int
anioVigilante (_ , _ , anio) = anio
--b
muerte :: Vigilante -> Evento
muerte muerto vigilantes= filter (\vigilante -> nombreVigilante vigilante /= nombreVigilante muerto) vigilantes

drManhattan :: Vigilante
drManhattan = ("Dr. Manhattan" , [] , 1990)

rorschach :: Vigilante
rorschach = ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964)
--a
destruccionNiuShork :: Evento
destruccionNiuShork vigilantes = muerte rorschach . muerte drManhattan $ vigilantes
--c
agregarHabilidad :: String -> Vigilante -> Vigilante
agregarHabilidad ability (name, abilities , year) = (name , ability : abilities , year)

efectosVietnam :: Vigilante -> Vigilante
efectosVietnam vigilante 
    |esAgenteGobierno vigilante = agregarHabilidad "cinismo" vigilante
    |otherwise = vigilante

type AgenteDelGobierno = (String , String)
agentesDelGobierno :: [AgenteDelGobierno]
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

esAgenteGobierno :: Vigilante -> Bool
esAgenteGobierno vigilante = any (\agente -> fst agente == nombreVigilante vigilante) agentesDelGobierno

guerraDeVietnam :: [Vigilante] -> [Vigilante]
guerraDeVietnam  = map efectosVietnam

--d
accidenteLaboratorio :: Int -> Evento
accidenteLaboratorio anio vigilantes = ("Dr. Manhattan", ["manipulacion de materia a nivel atomico"], anio) : vigilantes

--e
--actaDeKeene :: [Vigilante] -> Evento
--actaDeKeene nuevosVigilantes [] = nuevosVigilanes
--actaDeKeene nuevosVigilantes (vigilante : vigilantes)
 --   | elem vigilantes nuevosVigilanes = actaDeKeene (reemplazarVigilante vigilante nuevosVigilantes) vigilantes
 --   | otherwise = actaDeKeene (vigilante : nuevosVigilanes) vigilantes


--vigilanteMasJoven :: Vigilante -> Vigilante -> Vigilante
--vigilanteMasJoven vigilante vigilant
 --   | anio vigilante > anio vigilant = vigilante
 --   | otherwise = vigilante

--reemplazarVigilante :: Vigilante -> [Vigilante] -> [Vigilante]
--reemplazarVigilante vigilante vigilantes = takeWhile ((/=(nombreVigilante vigilante) . nombreVigilante)) vigilantes ++ (vigilanteMasJoven vigilante head . dropWhile (\vigilador -> nombreVigilante vigilador /= nombreVigilante vigilante) vigilantes) ++ drop 1 (dropWhile (\vigilador -> nombreVigilante vigilador /= nombreVigilante vigilante) vigilantes)


--1
historia :: [Vigilante] -> [Evento] -> [Vigilante]
historia vigilantes [] = vigilantes
historia vigilantes (evento : eventos) = historia (evento vigilantes) eventos

actaDeKeene :: Evento
actaDeKeene pe = pe

historiaPapu :: [Vigilante] -> [Vigilante]
historiaPapu vigilantes=  historia vigilantes [actaDeKeene , accidenteLaboratorio 1949 , guerraDeVietnam , destruccionNiuShork]


--2
vigilanteSinHabilidades :: Vigilante
vigilanteSinHabilidades = ("SinHabilidades" , [] , 0)

masHabilidades :: Vigilante -> Vigilante -> Vigilante
masHabilidades vig1 vig2 
    | (length . habilidadesVigilante $ vig1) >= (length . habilidadesVigilante $ vig2) = vig1
    | otherwise = vig2

nombreDelSalvador :: [Vigilante] -> Vigilante
nombreDelSalvador vigilantes = foldl masHabilidades (head vigilantes) vigilantes


--b
elElegido :: [Vigilante] -> String
elElegido vigilantes = head . habilidadesVigilante $ (foldr masPalabras vigilanteSinHabilidades (guerraDeVietnam vigilantes))


masPalabras :: Vigilante -> Vigilante -> Vigilante
masPalabras v1 v2
    | (sum . map palabraAnumero . nombreVigilante $ v1) > (sum . map palabraAnumero . nombreVigilante $ v2) = v1
    | otherwise                                                   = v2

palabraAnumero :: Char -> Int
palabraAnumero ' ' = 1
palabraAnumero _   = 0

--c
masViejo :: Vigilante -> Vigilante -> Vigilante
masViejo v1 v2 
    | anioVigilante v1 > anioVigilante v2 = v1
    | otherwise = v2

patriarca :: [Vigilante] -> Vigilante
patriarca vigilantes = foldl masViejo vigilanteSinHabilidades . actaDeKeene $ vigilantes

