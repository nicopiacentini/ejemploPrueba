--1
type Gema = Personaje -> Personaje
data Guantelete = UnGuantelete{
    material :: String,
    gemas :: [Gema]
}

data Personaje = UnPersonaje{
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    planetaActual :: String
}
type Universo = [Personaje]
--2
aptoParaPendex :: Universo -> Bool
aptoParaPendex = any (< 45) . map edad
tieneAlmenosUnaHabilidad :: Personaje -> Bool
tieneAlmenosUnaHabilidad = (> 0) . length . habilidades

energiaTotal :: Universo -> Float
energiaTotal  = sum . map energia . filter tieneAlmenosUnaHabilidad

--3
modificarEnergia :: Float -> Personaje -> Personaje
modificarEnergia nuevaEnergia personaje = personaje{energia = nuevaEnergia + energia personaje}

mente :: Float -> Gema
mente valorDebilitado personaje = modificarEnergia (-valorDebilitado) personaje

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidad personaje= personaje{habilidades = filter (/=habilidad) (habilidades personaje)}

alma :: String -> Gema
alma habilidad personaje= modificarEnergia (- 10) . quitarHabilidad habilidad $ personaje

cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta nuevoPlaneta personaje= personaje{planetaActual = nuevoPlaneta}

espacio :: String -> Gema
espacio nuevoPlaneta = modificarEnergia (-20) . cambiarPlaneta nuevoPlaneta

quitarHabilidadesSiTieneMenosDe :: Int -> Personaje -> Personaje
quitarHabilidadesSiTieneMenosDe cantidad personaje
    | cantidadHabilidades < cantidad = personaje{habilidades = []}
    | otherwise = personaje
    where cantidadHabilidades = length . habilidades $ personaje

poder :: Gema
poder personaje = modificarEnergia (- energia personaje) . quitarHabilidadesSiTieneMenosDe 2 $ personaje

modificarEdad :: Int -> Personaje -> Personaje
modificarEdad nuevaEdad personaje = personaje{edad = nuevaEdad}

tiempo :: Gema
tiempo personaje = modificarEnergia (-50) . modificarEdad ( max 18 (div (edad personaje) 2)) $ personaje

gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema
--4

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = UnGuantelete "goma" [tiempo , alma "usar mjolnir" , gemaLoca (alma "programacion en Haskell")]

--5
utilizar :: Personaje -> [Gema] -> Personaje
utilizar personaje gemas = foldl (\persona gema -> gema persona) personaje gemas

--6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = gemaMasPoderosaDe personaje . gemas $ guantelete

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1 : gema2 : gemas)
    |energia (gema1 personaje) > energia (gema2 personaje) = gemaMasPoderosaDe personaje (gema1 : gemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2 : gemas)


--7
