data Jugador = CJugador{
    nombre :: String,
    edad :: Int, 
    promedioDeGol :: Float,
    habilidad :: Int,
    cansancio :: Float
}deriving Show
type Equipo = (String , Char , [Jugador])
martin :: Jugador
martin = CJugador "Martin" 26 0.0 50 35.0
juan :: Jugador
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi :: Jugador
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0

jonathan :: Jugador
jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean :: Jugador
lean = CJugador "Hacha" 23 0.01 50 35.0
brian :: Jugador
brian = CJugador "Panadero" 21 5 80 15.0

garcia :: Jugador
garcia = CJugador "Sargento" 30 1 80 13.0
messi :: Jugador
messi = CJugador "Pulga" 26 10 99 43.0
aguero :: Jugador
aguero = CJugador "Aguero" 24 5 90 5.0

equipo1 :: Equipo
equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre :: Equipo
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo :: Equipo
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

--1
jugadoresDelEquipo :: Equipo -> [Jugador]
jugadoresDelEquipo (_,_,j)=j
esHabilidoso :: Jugador -> Bool
esHabilidoso unjugador = (> 75) . habilidad $ unjugador
promedioPositivo :: Jugador -> Bool
promedioPositivo jugador = (> 0) . promedioDeGol $ jugador
esFigura :: Jugador -> Bool
esFigura jugador = (esHabilidoso jugador) && (promedioPositivo jugador)

figurasDeUnEquipo :: Equipo -> [Jugador]
figurasDeUnEquipo equipo = filter esFigura . jugadoresDelEquipo $ equipo
