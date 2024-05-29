data Persona = UnaPersona{
    nombrePersona :: String,
    dineroActual :: Float, 
    suerte :: Int,
    factores :: [Factor]
}deriving Show
type Factor = (String , Int)
nico :: Persona
nico = (UnaPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu :: Persona
maiu = (UnaPersona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])
--1
nombreFactor :: Factor -> String
nombreFactor = fst
numeroFactor :: Factor -> Int
numeroFactor = snd

valorAmuleto :: [Factor] -> Int
valorAmuleto [] = 0
valorAmuleto (factor : factores)
    |(== "amuleto") . nombreFactor $ factor = numeroFactor factor
    |otherwise = valorAmuleto factores
suerteTotal :: Persona -> Int
suerteTotal persona
    |tieneFactor "amuleto" persona = (suerte persona) * (valorAmuleto (factores persona))
    |otherwise = suerte persona

--2
data Juego = UnJuego{
    nombreJuego :: String,
    dineroGanado :: Float -> Float,
    criterios :: [Criterio]
}
type Criterio = Persona -> Bool

suerteMayorA :: Int -> Persona -> Bool
suerteMayorA numero = (> numero) . suerte

ruleta :: Juego
ruleta = UnJuego "La Ruleta" (37 *) [(suerteMayorA 80)]

jackpot :: Float
jackpot = 4

tieneFactor :: String -> Persona -> Bool
tieneFactor factor persona = any (== factor) . map nombreFactor . factores $ persona

maquinita :: Juego
maquinita = UnJuego "La Maquinita" (jackpot +) [(suerteMayorA 95), tieneFactor "paciencia"]

--3

puedeGanar :: Juego -> Persona -> Bool
puedeGanar juego persona = and . map ($ persona) . criterios $ juego
--4
--a
timbea :: Float -> Juego -> Float
timbea apuesta juego = (dineroGanado juego) apuesta
timbeaEnMaquinas :: Persona -> Float -> [Juego] -> Float
timbeaEnMaquinas persona apuesta juegos = foldl (timbea) apuesta . filter (\juego -> puedeGanar juego persona) $ juegos


--b
modificarDinero :: Persona -> Float -> Persona
modificarDinero persona dineroNuevo = persona{dineroActual  = dineroNuevo + dineroActual persona }
timbeaEnMaquinasRecursiva :: Persona -> Float -> [Juego] -> Float
timbeaEnMaquinasRecursiva persona _ []= dineroActual persona
timbeaEnMaquinasRecursiva persona apuestaInicial (juego : juegos) 
    | puedeGanar juego persona = timbeaEnMaquinasRecursiva persona ((dineroGanado juego) apuestaInicial) juegos
    | otherwise = timbeaEnMaquinasRecursiva persona apuestaInicial juegos

--5
noGanaNingunJuego :: [Juego] -> Persona -> Bool
noGanaNingunJuego juegos persona = null . filter (\juego -> puedeGanar juego persona) $ juegos
noGananNinguno :: [Persona] -> [Juego] -> [Persona]
noGananNinguno jugadores juegos = filter (noGanaNingunJuego juegos) jugadores

--6
timbear :: Persona -> Float -> Juego -> Persona
timbear persona dineroApostado juego
    | puedeGanar juego persona = modificarDinero persona (((dineroGanado juego) dineroApostado) - dineroApostado)
    | otherwise = modificarDinero persona (-dineroApostado)

--7
elCocoEstaEnLaCasa :: (Ord b, Num b, Foldable t1) => (a, [b]) -> (t2 -> [b]) -> b -> t1 ([b] -> [b], t2) -> Bool
elCocoEstaEnLaCasa x y z = all ((>z).(+42)).foldl (\a (b,c) -> y c ++ b a) (snd x)

