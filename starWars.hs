import Text.Show.Functions()
--1
data Nave = UnaNave {
    nombreNave :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: PoderNave
}deriving Show
type PoderNave = Nave -> Nave

{-Nombre: TIE Fighter
Durabilidad: 200
Escudo: 100
Ataque: 50
Poder:  Hace un movimiento Turbo, el cual incrementa su ataque en 25-}
tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 turbo

turbo :: PoderNave
turbo = modificarAtaque 25 

modificarAtaque :: Int -> Nave -> Nave
modificarAtaque diferencia nave = nave{ataque = diferencia + ataque nave}

{-Nombre: X Wing
Durabilidad: 300
Escudo: 150
Ataque: 100
Poder: Hace una reparación de emergencia, lo cual aumenta su durabilidad en 50 pero reduce su ataque en 30.-}

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 reparacionDeEmergencia

reparacionDeEmergencia :: PoderNave
reparacionDeEmergencia nave = modificarDurabilidad 50 . modificarAtaque (- 30) $ nave

modificarDurabilidad :: Int -> Nave -> Nave
modificarDurabilidad diferencia nave = nave{durabilidad = diferencia + durabilidad nave} 

{-Nombre: Nave de Darth Vader
Durabilidad: 500
Escudo: 300
Ataque: 200
Poder: Hace un movimiento Super Turbo, lo cual significa hacer 3 veces el movimiento Turbo y reducir la durabilidad en 45.
-}

naveDeDarthVader :: Nave
naveDeDarthVader = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo

superTurbo :: PoderNave
superTurbo = turbo . turbo . turbo . modificarDurabilidad (- 30)

{-Nombre: Millennium Falcon
Durabilidad: 1000
Escudo: 500
Ataque: 50
Poder: Hace una reparación de emergencia y además se incrementan sus escudos en 100-}

modificarEscudos :: Int -> Nave -> Nave
modificarEscudos diferencia nave = nave{escudo = diferencia + escudo nave}

millenniumFalcon :: Nave
millenniumFalcon = UnaNave{
    nombreNave = "Millennium Falcon",
    durabilidad = 1000,
    escudo = 500,
    ataque = 50,
    poder = reparacionDeEmergencia . modificarEscudos 100  
}

--2
type Flota = [Nave]

durabilidadTotal :: Flota -> Int
durabilidadTotal flota = sum . map durabilidad $ flota

--3
danioRecibido :: Nave -> Nave -> Int
danioRecibido atacante atacada = (escudo atacada) - (ataque atacante)
atacar :: Nave -> Nave -> Nave
atacar atacante atacada 
    | (escudo . (poder atacada) $ atacada) > (ataque . (poder atacante) $ atacante) = modificarDurabilidad (max 0 (danioRecibido ((poder atacante) atacante) ((poder atacada) atacada))) atacada
    | otherwise = atacada

--4
estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate nave = (== 0) . durabilidad $ nave

--5

type Estrategia = Nave -> Bool

flotaDespuesDeAtaque :: Flota -> Estrategia -> Nave -> Flota
flotaDespuesDeAtaque [] _ _ = []
flotaDespuesDeAtaque (naveEnemiga : navesEnemigas) estrategia naveAtacante
    | estrategia naveEnemiga = atacar naveAtacante naveEnemiga : (flotaDespuesDeAtaque navesEnemigas estrategia naveAtacante)
    | otherwise = naveEnemiga : (flotaDespuesDeAtaque navesEnemigas estrategia naveAtacante)

naveDebiles :: Estrategia
naveDebiles nave = (< 200) . escudo $ nave

naveConPeligrosidad :: Int -> Estrategia
naveConPeligrosidad ataqueMaximo = (ataqueMaximo <) . ataque

naveFueraDeCombate :: Nave -> Estrategia
naveFueraDeCombate naveAtacante naveAtacada = estaFueraDeCombate . atacar naveAtacante $ naveAtacada

noEscudo :: Nave -> Estrategia
noEscudo naveAtacante = (== 0) . max 0 . escudo . atacar naveAtacante 

--6
durabilidadConEstrategia :: Flota -> Nave -> Estrategia -> Int
durabilidadConEstrategia flota naveAtacante estrategia = durabilidadTotal . (flotaDespuesDeAtaque flota estrategia) $ naveAtacante

laMejorEstrategia :: Flota -> Nave -> Estrategia -> Estrategia -> Flota
laMejorEstrategia flota naveAtacante estrategia1 estrategia2
    | (durabilidadConEstrategia flota naveAtacante estrategia1) > ((durabilidadConEstrategia flota naveAtacante estrategia2)) = flotaDespuesDeAtaque flota estrategia1 naveAtacante
    | otherwise = flotaDespuesDeAtaque flota estrategia2 naveAtacante

--7

flotaInfinita :: Flota
flotaInfinita = cycle [xWing , naveDeDarthVader , millenniumFalcon , tieFighter]

{-
Por ser una flota infinita, tiene infinitas naves y la suma de todas las durabilidades DIVERGE
por lo que no se mostrara nada en pantalla
-}