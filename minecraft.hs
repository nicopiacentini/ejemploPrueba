data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Int,
    inventario:: [Material]
} deriving (Show,Eq)

data Receta = UnaReceta{
    materialesNecesarios :: [Material],
    segundos :: Int
}deriving (Show,Eq)
data Material = Objeto {nombreObjeto :: String, recetaObjeto :: Receta} | Material {nombreMaterial :: String} deriving (Show,Eq)

madera :: Material
madera = Material "madera"

recetaPalo :: Receta
recetaPalo = UnaReceta [madera,madera] 10 

palo :: Material
palo = Objeto "palo" recetaPalo

piedra :: Material
piedra = Material "piedra"

steve :: Personaje
steve = UnPersonaje "Steve" 0 [madera, madera, piedra]

quitarDeInventario :: [Material] -> [Material] -> [Material]
quitarDeInventario inventario [] = inventario
quitarDeInventario inventario (material:materiales) = quitarDeInventario (takeWhile (/= material) inventario ++ drop 1 (dropWhile (/= material) inventario)) materiales

craftearConMateriales :: Personaje -> Material -> Personaje
craftearConMateriales (UnPersonaje nombreP puntajeP inventarioP) material = UnPersonaje{
    nombre = nombreP,
    puntaje = (puntajeP + ). (10 * ) . segundos . recetaObjeto $ material,
    inventario = (material :) . quitarDeInventario inventarioP . materialesNecesarios . recetaObjeto $ material
}

intersectar :: Eq a => [a] -> [a] -> [a]
intersectar _ [] = []
intersectar [] _ = []
intersectar xs (y:ys)
    | y `elem` xs = y :intersectar xs ys
    | otherwise =  intersectar xs ys

tieneMateriales :: [Material] -> Personaje -> Bool
tieneMateriales materiales = (== materiales) . intersectar materiales . inventario 


crafteo :: Personaje -> Material -> Personaje
crafteo personaje material
    | tieneMateriales (materialesNecesarios . recetaObjeto $ material) personaje = craftearConMateriales personaje material
    | otherwise = personaje {puntaje = puntaje personaje - 100}



crafteablesDeLaLista :: Personaje -> [Material] -> [Material]
crafteablesDeLaLista _ [] = []
crafteablesDeLaLista personaje (material : materiales) 
    | tieneMateriales necesarios personaje = material : crafteablesDeLaLista (craftearConMateriales personaje material) materiales
    | otherwise = crafteablesDeLaLista personaje materiales
    where necesarios = (materialesNecesarios . recetaObjeto $ material)

duplicaPuntaje :: Personaje -> Material -> Bool
duplicaPuntaje personaje material = puntaje (crafteo personaje material) > 2 * puntaje personaje
duplicanYCrafteables :: Personaje -> [Material] -> [Material]
duplicanYCrafteables personaje materiales = filter (\material -> duplicaPuntaje personaje material) (crafteablesDeLaLista personaje materiales)


craftearTodos :: Personaje -> [Material] -> Personaje
craftearTodos personaje materiales = foldl crafteo personaje materiales

masPuntos :: Personaje -> [Material] -> [Material]
masPuntos personaje materiales
    | (puntaje . craftearTodos personaje $ materiales) > (puntaje . craftearTodos personaje . reverse $ materiales) = materiales
    | otherwise = reverse materiales


--2
data Bioma = UnBioma{
    nombreBioma :: String,
    objetosBioma :: [Material],
    elementoBioma :: Material
}

type Herramienta = Bioma -> Material

tieneElemento :: Personaje -> Bioma -> Bool
tieneElemento personaje bioma = elem (elementoBioma bioma) (inventario personaje)

minar :: Personaje -> Herramienta -> Bioma -> Personaje
minar personaje herramienta bioma
    | tieneElemento personaje bioma = personaje{inventario = herramienta bioma : inventario personaje}
    | otherwise = personaje 

hacha :: Herramienta
hacha = last . objetosBioma

espada :: Herramienta
espada = head . objetosBioma

pico :: Int -> Herramienta
pico intensidad bioma = objetosBioma bioma !! (intensidad + 1)

picoMedio :: Bioma -> Material
picoMedio bioma = pico (div (length (objetosBioma bioma)) 2) bioma