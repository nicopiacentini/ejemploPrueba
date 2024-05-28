type Jugador = (String , String , Habilidades)
type Fuerza = Int
type Presicion = Int
type Habilidades = (Fuerza , Presicion)
bart :: Jugador
bart = ("Bart","Homero",(25,60))
todd :: Jugador
todd = ("Todd","Ned",(15,80))
rafa :: Jugador
rafa = ("Rafa","Gorgory",(10,1))
nombre :: Jugador -> String
nombre (n,_,_) = n
padre :: Jugador -> String
padre (_,p,_) = p
habilidad :: Jugador -> Habilidades
habilidad (_,_,h) = h

type Velocidad = Int
type Altura = Int
type Tiro = (Velocidad , Presicion , Altura)

type Obstaculo = (Tiro -> Bool , Tiro -> Tiro)

laguna :: Int -> Obstaculo
laguna largo = ((\(v,_,a)-> v>80 && between 10 50 a),(\(v,p,a) -> (v,p,a `div` largo)))
tunelConRampita :: Obstaculo
tunelConRampita = ((\(_,p,a) -> p>90 && a==0), (\(v,_,_) -> (v*2,100,0)) )
hoyo :: Obstaculo
hoyo = ((\(v,p,a) -> between 5 20 v && p>95 && a==0), (\ _ -> (0,0,0)))
between :: Int -> Int -> Int -> Bool
between n m x = elem x [n .. m]
maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b 
    | f a >= f b = a
    | otherwise = b

type Palo = Habilidades -> Tiro

presicion :: Habilidades -> Presicion
presicion (_ , p) = p
fuerza:: Habilidades -> Fuerza
fuerza (f, _) = f

putter :: Palo
putter habilidades = (10 , (2 *) . presicion $ habilidades, 0)

madera :: Palo
madera habilidades =(100 , (`div` 2) . presicion $ habilidades, 5)

hierro :: Int -> Palo
hierro n habilidades = ((n *) . fuerza $ habilidades , (`div` n) . presicion $ habilidades , n * n)

palos :: [Palo]
palos =  putter : madera : map hierro [1 .. 10]

--2
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo . habilidad $ jugador 

--b
condicion :: Obstaculo -> (Tiro -> Bool)
condicion = fst
puedeSuperar :: Obstaculo -> Tiro -> Bool
puedeSuperar obstaculo = (condicion obstaculo)

--c
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (puedeSuperar obstaculo . golpe jugador) palos

--d
pasaTodosLosObstaculos :: [Obstaculo] -> Jugador -> Bool
pasaTodosLosObstaculos obstaculos jugador = and . map (not . null . palosUtiles jugador) $ obstaculos
nombresDeLosQuePuedenSuperarTodos :: [Obstaculo] -> [Jugador] -> [String]
nombresDeLosQuePuedenSuperarTodos obstaculos jugadores = map nombre . filter (pasaTodosLosObstaculos obstaculos) $ jugadores

--3
cuantosObstaculosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosSupera _ [] = 0
cuantosObstaculosSupera tiro (obstaculo : obstaculos)
    | puedeSuperar obstaculo tiro = 1 + cuantosObstaculosSupera ((snd obstaculo) tiro) obstaculos
    | otherwise = 0

--b
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil persona obstaculos = foldl (paloQueMasLlega persona obstaculos) (head palos) palos

paloQueMasLlega :: Jugador -> [Obstaculo] -> Palo -> Palo -> Palo
paloQueMasLlega jugador obstaculos palo1 palo2
    |(cuantosObstaculosSupera (golpe jugador palo1) obstaculos) > (cuantosObstaculosSupera (golpe jugador palo2) obstaculos) = palo1
    | otherwise = palo2

--4
puntosGanados :: [Obstaculo] -> Jugador -> Int
puntosGanados _ jugador = fuerza . habilidad $ jugador 


ganadores :: [Jugador] -> [Obstaculo] -> [Jugador]
ganadores jugadores obstaculos= (ordenarPorPuntaje obstaculos) . terminaronElCircuito obstaculos $ jugadores
pierdenLaApuesta :: [Jugador] -> [Obstaculo] -> [String]
pierdenLaApuesta jugadores obstaculos = map padre . filter (\jugador -> not(elem jugador (ganadores jugadores obstaculos))) $ jugadores 

terminaronElCircuito :: [Obstaculo] -> [Jugador] -> [Jugador]
terminaronElCircuito _ [] = []
terminaronElCircuito obstaculos (jugador : jugadores) 
    | (length obstaculos) == cuantosObstaculosSupera (golpe jugador (paloMasUtil jugador obstaculos)) obstaculos = jugador : terminaronElCircuito obstaculos jugadores
    | otherwise = terminaronElCircuito obstaculos jugadores

mayorPuntaje :: [Jugador] -> [Obstaculo] -> Int
mayorPuntaje jugadores obstaculos= maximum . map (puntosGanados obstaculos) $ jugadores

tieneMayorPuntaje :: Jugador -> [Jugador] -> [Obstaculo] -> Bool
tieneMayorPuntaje jugador jugadores obstaculos = (puntosGanados obstaculos jugador) == (mayorPuntaje jugadores obstaculos)
ordenarPorPuntaje :: [Obstaculo] -> [Jugador] -> [Jugador]
ordenarPorPuntaje obstaculos jugadores = filter (\jugador -> (mayorPuntaje jugadores obstaculos) == (puntosGanados obstaculos jugador)) jugadores


    