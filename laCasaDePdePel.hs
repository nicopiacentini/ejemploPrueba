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
rio = UnLadron "rio" ["hola" , "si"] [ametralladora 3]
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


