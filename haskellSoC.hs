
data Propuesta = UnaPropuesta{
    nombreAlumno :: String, 
    propuesta :: [NombrePropuesta],
    skills :: [String],
    anios :: Int
} deriving (Eq,Show)
type NombrePropuesta = String
type CriterioMentor = Propuesta -> Int

data Mentor = UnMentor{
    nomrbeMentor :: String,
    propuestasInteresado :: [Propuesta],
    criterio :: CriterioMentor
}

data Resultado = UnResultado{
    nombreA :: String,
    proyecto :: Propuesta,
    puntos :: Int
}

leInteresaPropuesta :: Propuesta -> Mentor -> Bool
leInteresaPropuesta propuesta mentor  = elem propuesta . propuestasInteresado $ mentor

puntosSegun :: Mentor -> Propuesta -> Int
puntosSegun mentor propuesta
    |leInteresaPropuesta propuesta mentor = (1 +) . criterio mentor $ propuesta
    | otherwise = (criterio mentor) propuesta


---2  
--puntajeTotal/2 que recibe una propuesta, una lista de mentores y 
--devuelve el puntaje total que ésta obtendría a partir de las votaciones de todos los mentores.

criterioMentor :: Mentor -> CriterioMentor 
criterioMentor = criterio 

puntajeTotal :: Propuesta -> [Mentor] -> Int
puntajeTotal propuesta = sum . map ($ propuesta) . map criterio  


--3 propuestasConChances/1 que recibe una lista de propuestas y 
--devuelve las que indican que el alumno tiene más de 3 skills.

cantidadDeSkills :: Propuesta -> Int
cantidadDeSkills propuesta = length . skills $ propuesta

propuestasConChances :: [Propuesta] -> [Propuesta]
propuestasConChances propuestas = filter (\propuesta -> (> 3) . cantidadDeSkills $ propuesta) propuestas


--4
{-ranking/2 que devuelve una lista de resultados a partir de una lista de mentores y una lista de propuestas.
Los resultados son representados por  3-uplas  (nombreDeAlumno,proyecto,puntajeTotal) sobre todas las propuestas recibida-}

--ordenarPorMentor :: Mentor -> [Propuesta] -> [Propuesta]
--ordenarPorMentor mentor propuestas = 

--ranking :: [Mentor] -> [Propuesta] -> [Propuesta]
--ranking [] propuestas = propuestas
--ranking (mentor : mentores) propuestas = ranking mentores (ordenarPorMentor mentor propuestas)


--5
propuestasDeInteres :: [Propuesta] -> Mentor -> [Propuesta]
propuestasDeInteres propuestas mentor = filter (\propuesta -> leInteresaPropuesta propuesta mentor) propuestas

--7

nombreMentorMasInteresado :: Propuesta -> [Mentor] -> String
nombreMentorMasInteresado _ mentor = mentor
nombreMentorMasInteresado propuesta (mentor1 : mentor2 : mentores)
    | (criterio mentor1) propuesta > (criterio mentor2) propuesta = nombreMentorMasInteresado propuesta (mentor1 : mentores)
    | otherwise = nombreMentorMasInteresado propuesta (mentor2 : mentores)

--8