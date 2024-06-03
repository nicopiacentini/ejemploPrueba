data Elemento = UnElemento { 
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje)
}
data Personaje = UnPersonaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
}

--1
modificarAnio :: Personaje -> Int -> Personaje
modificarAnio personaje anio = personaje{anioPresente = anio}

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = modificarAnio personaje anio

modificarSalud :: Personaje -> Float -> Personaje
modificarSalud personaje saludNueva = personaje{salud = saludNueva}

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud personaje (1.5 * salud personaje)

causarDanio :: Personaje -> Float -> Personaje
causarDanio personaje danio = modificarSalud personaje (max 0 (salud personaje - danio))


--2
malvado :: Elemento -> Bool
malvado elemento = "malvado" == tipo elemento
esMalvado :: Personaje -> Bool
esMalvado personaje = any malvado . elementos $ personaje

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = max 0 . (salud personaje -) . salud . ataque elemento $ personaje

puedeMatarlo :: Personaje -> Personaje -> Bool
puedeMatarlo asesinado asesino = any (==0) . map (danioQueProduce asesinado) . elementos $ asesino

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (puedeMatarlo personaje) enemigos

--3
noAtaca :: Personaje -> Personaje
noAtaca personaje = personaje

noDefiende :: Personaje -> Personaje
noDefiende personaje = personaje

meditarVeces :: Int -> Personaje -> Personaje
meditarVeces 0 personaje = personaje
meditarVeces n personaje = meditarVeces (n-1) (meditar personaje)
concentracion :: Int -> Elemento
concentracion nivelDeConcentracion = UnElemento{
    tipo = "Magia",
    ataque = noAtaca,
    defensa = meditarVeces nivelDeConcentracion
}

esbirro :: Elemento
esbirro = UnElemento{
    tipo = "Malvado",
    ataque = (`causarDanio` 1),
    defensa = noDefiende
}

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirro

katanaMagica :: Elemento
katanaMagica = UnElemento{
    tipo = "Magia",
    ataque = (`causarDanio` 1),
    defensa = noDefiende
}

jack :: Personaje
jack = UnPersonaje{
    nombre = "Jack",
    salud = 300,
    elementos = [concentracion 3, katanaMagica],
    anioPresente = 200
}

--d
portalAlFututo :: Int -> Elemento
portalAlFututo anioActual = UnElemento{
    tipo = "Magia",
    ataque = mandarAlAnio (2800 + anioActual),
    defensa = noDefiende
}
aku :: Int -> Float -> Personaje
aku anio salud' = UnPersonaje{
    nombre = "aku",
    salud = salud',
    elementos = [concentracion 4 , portalAlFututo anio] ++ esbirrosMalvados (100 * anio),
    anioPresente = anio
}

atacar :: Personaje -> Personaje -> Personaje -- Me dice como queda el atacado
atacar atacante atacado = foldr ($) (atacado) (map ataque (elementos atacante))

defenderse :: Personaje -> Personaje
defenderse personaje = foldr ($) personaje (map defensa (elementos personaje))

--4
luchar :: Personaje -> Personaje -> (Personaje , Personaje)
luchar atacante defensor
   | (== 0) . salud . atacar atacante . defenderse $ defensor = (atacante , defensor)
   | otherwise = luchar (atacar atacante (defenderse defensor)) atacante


--5
f :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))