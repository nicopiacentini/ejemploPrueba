type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre


esVocal :: Char -> Bool
esVocal = flip elem "aeiou"


tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"




cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)


--1 
type Rima = Palabra -> Palabra -> Bool
ultimasCumplen :: Int -> (Char -> Bool) -> Palabra -> String
ultimasCumplen numero condicion palabra = take numero . reverse . filter condicion $ palabra

nada :: Char -> Bool
nada _ = True

esRimaAsonante :: Rima
esRimaAsonante palabra1  = (== (ultimasCumplen 2 esVocal  palabra1)) . ultimasCumplen 2 esVocal

esRimaConsonante :: Rima
esRimaConsonante palabra1 = (== (ultimasCumplen 3 nada palabra1)) . ultimasCumplen 3 nada


riman :: Palabra -> Palabra -> Bool
riman palabra1 palabra2 = palabra1 /= palabra2 && (esRimaAsonante palabra1 palabra2 || esRimaConsonante palabra1 palabra2)


--b -> test


--2
ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words

primeraPalabra :: Verso -> Palabra
primeraPalabra = head . words

conjugacionRiman :: Verso -> Verso -> Bool
conjugacionRiman verso1 verso2 = conjugacionPalabra (ultimaPalabra verso1) (ultimaPalabra verso2) riman

conjugacionAnadiplosis :: Verso -> Verso -> Bool
conjugacionAnadiplosis verso1 verso2 = conjugacionPalabra (ultimaPalabra verso1) (primeraPalabra verso2) (==)

type CondicionConjugacion = Palabra -> Palabra -> Bool

conjugacionPalabra :: Palabra -> Palabra -> CondicionConjugacion -> Bool
conjugacionPalabra palabra1 palabra2 condicion = condicion palabra1 palabra2






verso_ :: Verso
verso_ = "no hace falta un programa que genere una canción"

otroVerso_ :: Verso
otroVerso_ = "para saber que esto se resuelve con una función"



--3
type TipoPatron = Estrofa -> Bool

ultimaPalabraVersoDeEstrofa :: Int -> Estrofa -> Palabra
ultimaPalabraVersoDeEstrofa posicion estrofa = ultimaPalabra . (estrofa !!) $ posicion

patronSimple :: Int -> Int -> TipoPatron
patronSimple posicion1 posicion2 estrofa = esRimaConsonante (ultimaPalabraVersoDeEstrofa posicion1 estrofa) . ultimaPalabraVersoDeEstrofa posicion2 $ estrofa 

esVocal' :: Char -> Bool
esVocal' = flip elem "aeiouáéíóú"

esEsdrujula :: Palabra -> Bool
esEsdrujula = tieneTilde . head . drop 2 . reverse . filter esVocal'

patronEsdrujula :: TipoPatron
patronEsdrujula = all (esEsdrujula). map (ultimaPalabra)

estrofaPrueba :: Estrofa
estrofaPrueba = ["a ponerse los guantes y subir al cuadrilátero","que después de este parcial acerca el paradigma lógico","no entiendo por qué está fallando mi código","si todas estas frases terminan en esdrújulas"]

sonTodosIguales :: [Palabra] -> Bool
sonTodosIguales palabras = all (== head palabras) palabras

patronAnafora :: TipoPatron
patronAnafora  = sonTodosIguales . map (head . words)


patronCadena :: CondicionConjugacion -> TipoPatron
patronCadena _ [] = True
patronCadena _ [_]= True
patronCadena condicionDeConjugacion [verso1,verso2] = condicionDeConjugacion verso1 verso2
patronCadena condicionDeConjugacion (verso1 : verso2 : versos) = condicionDeConjugacion verso1 verso2 && patronCadena condicionDeConjugacion (verso2 : versos)

patronCombinaDos :: TipoPatron -> TipoPatron -> TipoPatron
patronCombinaDos patron1 patron2 estrofa= patron1 estrofa && patron2 estrofa


--b
aabb :: TipoPatron
aabb = patronCombinaDos (patronSimple 3 4) (patronSimple 1 2)


abab :: TipoPatron
abab = patronCombinaDos (patronSimple 1 3) (patronSimple 2 4)
abba :: TipoPatron
abba =  patronCombinaDos (patronSimple 1 4) (patronSimple 2 3)

hardcore :: TipoPatron
hardcore = patronCombinaDos (patronCadena riman) (patronEsdrujula )

--c depende si patronCadena corta por un false entre los versos, retorna false en cambio si no lo hace no termina nunca porque la lista es infinita,
-- si lo ubiesemos puesto en otro orden , tambien depende de si patron esdrujula es en algun momento false. En conclucion a lo sumo son false

-- aabs si porque existen las posiciones necesarias para evaluar

--4
type Estilo = PuestaEnEscena -> PuestaEnEscena
data PuestaEnEscena = UnaEscena{
    estanExaltados :: Bool,
    potencia :: Float,
    estrofaFreestyle :: Estrofa, 
    artista :: Artista,
    estiloUtilizado :: Estilo
}

puestaBase :: Estrofa -> Artista -> Estilo -> PuestaEnEscena
puestaBase = UnaEscena False 1  

modificarPotencia :: Float -> PuestaEnEscena -> PuestaEnEscena
modificarPotencia nuevaPotencia escena = escena{potencia = nuevaPotencia}

gritar :: Estilo
gritar escena = modificarPotencia (1.5 * potencia escena) escena

modificarExalto :: Bool -> PuestaEnEscena -> PuestaEnEscena
modificarExalto exaltoNuevo escena = escena{estanExaltados = exaltoNuevo}

responderAcote :: Bool -> Estilo
responderAcote efectividad escena= modificarPotencia (1.2 * potencia escena) . modificarExalto efectividad $ escena

exaltarCondicional :: TipoPatron -> PuestaEnEscena -> PuestaEnEscena
exaltarCondicional patron puesta
    | patron . estrofaFreestyle $ puesta = modificarExalto True puesta
    | otherwise = puesta

tirarTecnicas :: TipoPatron -> Estilo
tirarTecnicas patron escena = exaltarCondicional patron . modificarPotencia (1.1 * potencia escena) $ escena

tirarFreestyle :: Estrofa -> Artista -> Estilo -> PuestaEnEscena
tirarFreestyle estrofa artista estilo = estilo . puestaBase estrofa artista $ estilo
 --5
type CriterioEscena = PuestaEnEscena -> Bool 


type Jurado = [(CriterioEscena , Float)]

alToke :: Jurado
alToke  = [((aabb . estrofaFreestyle) , 0.5) , (patronCombinaDos (patronEsdrujula) (patronSimple 1 4) . estrofaFreestyle , 1) , (estanExaltados , 1) , ((> 1.5) . potencia , 2)]


puntosTotales :: PuestaEnEscena -> Jurado -> Float
puntosTotales puesta = min 3 . sum . map snd . filter ( ($ puesta) . fst) 

--6
type Batalla = [PuestaEnEscena]
ganador :: Batalla -> Artista -> Artista -> [Jurado] -> Artista
ganador batalla artista1 artista2 jurados
    | puntos artista1 batalla jurados > puntos artista2 batalla jurados = artista1
    | otherwise = artista2

puntos :: Artista -> Batalla -> [Jurado] -> Float
puntos artista' batalla jurados= sum . map (puntear jurados) . filter ( (== artista') . artista) $ batalla


puntear jurados puestasEnEscena = foldr (\ juradosos puestaEnEscena -> map (puntosTotales puestaEnEscena) juradosos) jurados puestasEnEscena