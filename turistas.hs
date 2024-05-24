type Lugar = (String , Int , [String]) --(nombreLugar , distancia , atracciones y comida)
--Un producto es comestible cuando no tiene mayusculas
lugaresDeEjemplo :: [Lugar]
lugaresDeEjemplo = [("Rosario", 300,["Monumento a la bandera", "Rio"]), ("Mar Del Plata", 400, ["Playa", "alfajores", "Puloveres"]), ("Bariloche",1600, ["Montañas", "Nieve", "Puloveres", "chocolate"])]

type EstiloVacaciones = Lugar -> Bool
type Turista = (String , EstiloVacaciones)
juan :: Turista
juan = ("Juan", playero)
ana :: Turista
ana = ("Ana", mejorCerca)
jorge :: Turista
jorge = ("Jorge", gastronomico)
zulma :: Turista
zulma = ("Zulma", playero)

atraccionesLugar :: Lugar -> [String]
atraccionesLugar (_,_,a) = a

distanciaLugar :: Lugar -> Int
distanciaLugar (_,distancia,_) = distancia

nombreLugar :: Lugar -> String
nombreLugar (n,_,_) = n


playero :: EstiloVacaciones
playero (_ , _ , atracciones) = elem "Playa" atracciones 

mejorCerca :: EstiloVacaciones
mejorCerca lugar = (< 500) . distanciaLugar $ lugar

todoMinuscula :: String -> Bool
todoMinuscula = null . filter (\letra -> elem letra ['A' .. 'Z'])

gastronomico :: EstiloVacaciones
gastronomico lugar = any todoMinuscula . atraccionesLugar $ lugar

puedeIrNombre :: Turista -> [Lugar] -> [String]
puedeIrNombre unTurista = map nombreLugar . filter ((snd unTurista)) 

puedeIr :: Turista -> [Lugar] -> [Lugar]
puedeIr unTurista = filter (snd unTurista) 


lugarMasElegido :: [Turista] -> [Lugar] -> [Lugar]
lugarMasElegido [] lugares = lugares
lugarMasElegido (turista : turistas) lugares = lugarMasElegido turistas ( puedeIr turista lugares)


puedenIrTodos :: [Turista] -> Lugar -> Bool
puedenIrTodos turistas lugar =and . map ($ lugar). map snd $ turistas

pedro :: Turista
pedro = ("pedro" , (\lugar -> (> 5) . length . nombreLugar $ lugar))