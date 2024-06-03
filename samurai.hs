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

mandarAlAnio :: Personaje -> Int -> Personaje
mandarAlAnio personaje = modificarAnio personaje 

modificarSalud :: Personaje -> Float -> Personaje
modificarSalud personaje saludNueva = personaje{salud = saludNueva}

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud personaje (1.5 * salud personaje)

causarDanio :: Personaje -> Float -> Personaje
causarDanio personaje danio = modificarSalud personaje (max 0 (salud personaje - danio))