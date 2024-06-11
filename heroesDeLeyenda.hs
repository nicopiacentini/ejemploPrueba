data Heroe = UnHeroe{
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

type Tarea = Heroe -> Heroe


data Artefacto = Artefacto{
    nombreArtefacto :: String,
    rareza :: Int
}

--punto 2
lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "lanza del olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "xiphos" 50

modificarEpiteto :: String -> Heroe -> Heroe
modificarEpiteto apodo heroe = heroe {epiteto = apodo}

modificarArtefacto :: Artefacto -> Heroe -> Heroe
modificarArtefacto artefacto heroe = heroe {artefactos = artefacto : artefactos heroe}

pasaALaHistoria :: Heroe -> Heroe
pasaALaHistoria heroe
    | reconocimientoDelHeroe > 1000 = modificarEpiteto "El Mitico" heroe
    | reconocimientoDelHeroe >= 500 = modificarEpiteto "El Magnifico" . modificarArtefacto lanzaDelOlimpo $ heroe
    | reconocimientoDelHeroe > 100  = modificarEpiteto "Hoplita" . modificarArtefacto xiphos $ heroe
    | otherwise                     = heroe
    where reconocimientoDelHeroe = reconocimiento heroe

--punto 3
ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento cantidad heroe = heroe{reconocimiento = cantidad + reconocimiento heroe}

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto = ganarReconocimiento (rareza unArtefacto) . modificarArtefacto unArtefacto 
--escalar el olimpo

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza artefacto0 = Artefacto (nombreArtefacto artefacto0) (3 * rareza artefacto0) 

rarezaMayor :: Artefacto -> Bool
rarezaMayor (Artefacto _ raro) = raro >= 1000  

rempalagoDeZeus :: Artefacto
rempalagoDeZeus = Artefacto "relampago de zeus" 500

triplicarYdescartar :: [Artefacto] -> [Artefacto]
triplicarYdescartar artefactos =filter (rarezaMayor . triplicarRareza) artefactos

heroeTriplicarYDescartar :: Heroe -> Heroe
heroeTriplicarYDescartar heroe = heroe{artefactos = triplicarYdescartar (artefactos heroe)}

escalarElOlimpo :: Tarea
escalarElOlimpo = ganarReconocimiento 500 . heroeTriplicarYDescartar . modificarArtefacto rempalagoDeZeus 

--ayudar a cruzar la calle
ayudarCruzarLaCalle :: Int -> Tarea
ayudarCruzarLaCalle cuadras = modificarEpiteto ("Gros" ++ replicate cuadras 'o')
--matar una bestia
data Bestia = UnaBestia{
    nombreBestia :: String,
    debilidad :: Debilidad
}

type Debilidad = Heroe -> Bool

tirarUnArtefacto :: Heroe -> Heroe
tirarUnArtefacto heroe = heroe{artefactos = drop 1 (artefactos heroe)}

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe
    | debilidad bestia heroe = modificarEpiteto ("El asesino de " ++ (nombreBestia bestia)) heroe
    | otherwise              = (modificarEpiteto "El cobarde") . tirarUnArtefacto $ heroe

--4 

pistolaGriega :: Artefacto
pistolaGriega = Artefacto "fierro griego" 1000

heracles :: Heroe
heracles = UnHeroe{
    epiteto = "Guardian del olimpo",
    reconocimiento = 700,
    artefactos = [pistolaGriega,rempalagoDeZeus],
    tareas = [matarAlLeonDeNemea]
}

-------
-- 5 --
-------

leonDeNemea :: Bestia
leonDeNemea = UnaBestia "Leon de nemea" debilidadLeonDeNemea

debilidadLeonDeNemea :: Debilidad
debilidadLeonDeNemea = (>=20) . length . epiteto 

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia leonDeNemea

--6
agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea tarea heroe = heroe{tareas = tarea : tareas heroe}

realizarTarea :: Tarea -> Heroe -> Heroe
realizarTarea tarea = agregarTarea tarea . tarea 

--7
sumatoriaRarezasArtefactos :: Heroe -> Int
sumatoriaRarezasArtefactos  = sum . map (rareza) . artefactos

presumir :: Heroe -> Heroe -> (Heroe,Heroe)

presumir heroe1 heroe2
    | gana heroe1 heroe2 = (heroe1, heroe2)
    | gana heroe2 heroe1 = (heroe2,heroe1)
    | otherwise          = presumir (realizarTareasDe heroe1 heroe2) (realizarTareasDe heroe2 heroe1)

realizarTareasDe :: Heroe -> Heroe -> Heroe
realizarTareasDe unHeroe otroHeroe = realizarLabor (tareas otroHeroe) unHeroe

gana :: Heroe -> Heroe -> Bool
gana ganador perdedor = reconocimiento ganador > reconocimiento perdedor || reconocimiento ganador == reconocimiento perdedor && sumatoriaRarezasArtefactos ganador > sumatoriaRarezasArtefactos perdedor




--8
{-
Esta ejecucion de presumir nunca terminara de ejecutarse ya que al no ni tener tareas ni artefactos y tener igual reconocimiento,
cuando se le ejecutan las tareas de un a las del otro tanto los artefactos como los reconocimientos
se mantienen iguales y se vuelve a la situacion original, quedandose asi en un bucle infinito

-}

--9
realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor tareas heroe = foldl (flip realizarTarea) heroe tareas

--10
{-
No se podra conocer el estado final del heroe ya que hay infinitas tareas que cambian el estado del heroe

-}