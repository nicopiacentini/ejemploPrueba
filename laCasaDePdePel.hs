data Ladron = UnLadron{
    nombreLadron :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
} 

type Arma = Rehen -> Rehen

type Habilidad = String

data Rehen = UnRehen{
    nombreRehen :: String,
    nivelDeComplot :: Int,
    nivelDeMiedo :: Int,
    planContraLadrones :: Plan
} 

type Intimidacion = Ladron -> Rehen -> Rehen
type Plan = Rehen -> Ladron -> Ladron

cambiarNivelDeComplot :: Int -> Rehen -> Rehen
cambiarNivelDeComplot modificacion rehen = rehen{nivelDeComplot = modificacion + nivelDeComplot rehen}

cambiarMiedo :: Int -> Rehen -> Rehen
cambiarMiedo modificacion rehen = rehen{nivelDeMiedo = modificacion + nivelDeMiedo rehen}

pistola :: Int -> Arma
pistola calibre rehen = cambiarNivelDeComplot (- calibre * 5) . flip cambiarMiedo rehen . (3 *) . length . nombreRehen $ rehen 

ametralladora :: Int -> Arma
ametralladora balas rehen = cambiarNivelDeComplot (- div (nivelDeComplot rehen) 2) . cambiarMiedo balas $ rehen

disparos :: [Arma] -> Rehen -> Rehen
disparos armas rehen = ($ rehen) . elArmaQueMasConviene rehen $ armas

elArmaQueMasConviene :: Rehen -> [Arma] -> Arma
elArmaQueMasConviene _ [arma] = arma
elArmaQueMasConviene rehen (arma : arma' : armas)
    | (nivelDeMiedo . arma) rehen > (nivelDeMiedo . arma') rehen = elArmaQueMasConviene rehen (arma : armas)
    | otherwise = elArmaQueMasConviene rehen (arma' : armas)

elArmaQueMasConviene _ [] = pistola 3

hacerseElMalo :: Intimidacion
hacerseElMalo berlin = cambiarMiedo (cantidadDeLetrasDeHabilidades berlin)
hacerseElMalo rio = cambiarNivelDeComplot 20
hacerseElMalo _ = cambiarMiedo 10

rio :: Ladron
rio = UnLadron "rio" ["hola"] [ametralladora 3]
berlin :: Ladron
berlin = UnLadron "berlin" ["hola" , "si"] [ametralladora 3]

quitarArmas :: Int -> Ladron -> Ladron
quitarArmas cantidad ladron = ladron{armas = drop cantidad (armas ladron)}

puedeHacerElPlan :: Rehen -> Bool
puedeHacerElPlan rehen =  nivelDeComplot rehen > nivelDeMiedo rehen 

atacarAlLadron :: Rehen -> Plan
atacarAlLadron rehen otroRehen ladron
    | puedeHacerElPlan otroRehen= quitarArmas (length . nombreRehen $ rehen) ladron
    | otherwise = ladron

cantidadDeLetrasDeHabilidades :: Ladron -> Int
cantidadDeLetrasDeHabilidades  = length . concat . habilidades

esconderse :: Plan
esconderse rehen ladron
    | puedeHacerElPlan rehen = quitarArmas (div (cantidadDeLetrasDeHabilidades ladron) 3) ladron
    | otherwise = ladron

--1

tokio :: Ladron
tokio = UnLadron "tokio" ["trabajo psicologico" , "entrar en moto"] [pistola 9 , pistola 9 , ametralladora 30]


profesor :: Ladron
profesor = UnLadron "profesor" ["disfrasarce de linyera" , "disfrazarse de payaso" , "siempre estar un paso adelante"] []

pablo :: Rehen
pablo = UnRehen "pablo" 40 30 esconderse

arturito :: Rehen
arturito = UnRehen "arturito" 70 50 (atacarAlLadron pablo)

--2
esInteligente :: Ladron -> Bool
esInteligente (UnLadron "profesor" _ _) = True
esInteligente (UnLadron _ habilidades _) = length habilidades > 2

--3
conseguirUnArma :: Arma -> Ladron -> Ladron
conseguirUnArma arma ladron = ladron{armas = arma : armas ladron}

--4
intimidar :: Intimidacion -> Ladron -> Rehen -> Rehen
intimidar metodo ladron = metodo ladron 

--5
aplicarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
aplicarALosQueCumplen condicion funcion elementos = filter (not . condicion) elementos ++ (map funcion . filter (condicion) $ elementos)

calmarLasAguas :: Intimidacion -> Ladron -> [Rehen] -> [Rehen]
calmarLasAguas metodo ladron rehenes = aplicarALosQueCumplen (\rehen -> 60 > nivelDeMiedo rehen) (metodo ladron) rehenes


--6
nivelPromedio :: [Int] -> Int
nivelPromedio numeros = div (sum numeros) (length numeros)

cantidadDeArmas :: [Ladron] -> Int
cantidadDeArmas = sum . map length . map armas

pintaMalLaCosa :: [Ladron] -> [Rehen] -> Bool
pintaMalLaCosa ladrones rehenes = (nivelPromedio . map nivelDeComplot $ rehenes) > (cantidadDeArmas ladrones + (nivelPromedio . map nivelDeMiedo) rehenes )



--7
aplicarPlan :: Ladron -> Rehen -> Ladron
aplicarPlan ladron rehen = (planContraLadrones rehen) rehen ladron

rebelarse :: [Rehen] -> Ladron -> Ladron
rebelarse rehenes ladron = aplicarPlan ladron . head . map (cambiarNivelDeComplot (-10)) $ rehenes

