data Personaje = UnPersonaje{
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
}deriving (Eq , Show)

type Actividad = Personaje -> Personaje

lisa :: Personaje
lisa = UnPersonaje "Lisa simpson" 5 5

rafa :: Personaje
rafa = UnPersonaje "Rafa gorgory" 5 5

modificarFelicidad :: Int -> Personaje -> Personaje
modificarFelicidad modificacion persona = persona{felicidad = max 0 (modificacion + felicidad persona)}

modificarDinero :: Int -> Personaje -> Personaje
modificarDinero modificacion persona = persona{dinero = max 0 (modificacion + dinero persona)}

irALaEscuela :: Actividad
irALaEscuela lisa = modificarFelicidad 20 lisa
irALaEscuela persona = modificarFelicidad (-20) persona

comerXDonas :: Int -> Actividad
comerXDonas cantidad = modificarDinero (-10 * cantidad) . modificarFelicidad (10 * cantidad)

irATrabajar :: String -> Actividad
irATrabajar trabajo = modificarDinero (length trabajo)

trabajarComoDirector :: Actividad
trabajarComoDirector = irALaEscuela . irATrabajar "escuela elemental"

grafitiar :: Actividad
grafitiar = modificarDinero (-5)

--2
srBurns :: Personaje
srBurns = UnPersonaje "Burns" 50000000 0
type Logro = Personaje -> Bool
esMillonario :: Logro
esMillonario = (> dinero srBurns) . dinero

alegrarse :: Int -> Logro
alegrarse cantidad = (> cantidad) . felicidad

verAKrosti :: Logro
verAKrosti = (> 10) . dinero

esDecisivaParaElLogro :: Personaje -> Logro -> Actividad -> Bool
esDecisivaParaElLogro personaje logro actividad= ((not . logro $ personaje) &&) . logro . actividad $ personaje 

laDesiciva :: Personaje -> Logro -> [Actividad] -> Personaje
laDesiciva persona logro actividades 
    | hayAlgunaDecisiva persona logro actividades = (obtenerDesiciva persona logro actividades) persona
    | otherwise = persona

hayAlgunaDecisiva :: Personaje -> Logro -> [Actividad] -> Bool
hayAlgunaDecisiva personaje logro = any (\actividad -> esDecisivaParaElLogro personaje logro actividad)

obtenerDesiciva :: Personaje -> Logro -> [Actividad] -> Actividad
obtenerDesiciva personaje logro = head . filter (\actividad -> esDecisivaParaElLogro personaje logro actividad)

