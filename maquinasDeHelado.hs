data Helado = UnHelado{
    nombre :: String,
    temperatura :: Float,
    proporcionAgua :: Float
}

noCongelado :: Helado -> Bool
noCongelado helado = temperatura helado > 0

esVocalUno :: Char -> Float
esVocalUno letra  
    | letra `elem` "aeiouAEIOU" = 1
    | otherwise = 0
--

cantVocales :: String -> Float
cantVocales [] = 0
cantVocales (x:xs) = esVocalUno x + cantVocales xs

proporcionIncorrecta :: Helado -> Bool
proporcionIncorrecta (UnHelado "frutilla" _ proporcion) = proporcion /= 0.4
proporcionIncorrecta (UnHelado "Durazno" _ proporcion) = proporcion < 0.2 || proporcion > 0.6
proporcionIncorrecta (UnHelado nombre _ proporcion) 
    | cantLetras <= 8 = proporcion /= fromIntegral(cantLetras) / 10
    | otherwise = (cantVocales nombre)/10 /= proporcion
    where cantLetras = length nombre

salioMal :: Helado -> Bool
salioMal helado = noCongelado helado || proporcionIncorrecta helado


--punto2


heladera :: Float -> Helado -> Helado
heladera temperaturaReducida helado = helado {temperatura = temperatura helado - temperaturaReducida}

type CajonFruta = (String, Float)
frutaCajon :: CajonFruta -> String
frutaCajon = fst
kilosCajon :: CajonFruta -> Float
kilosCajon = snd

type BidonAgua = (Float, Float)
litrosAgua :: BidonAgua -> Float
litrosAgua = fst
temperaturaAgua :: BidonAgua -> Float
temperaturaAgua = snd

batidora :: BidonAgua -> CajonFruta -> Helado
batidora agua frutas = UnHelado{
    nombre = frutaCajon frutas,
    temperatura = temperaturaAgua agua,
    proporcionAgua = litrosAgua agua / kilosCajon frutas
}

exprimidora :: CajonFruta -> CajonFruta
exprimidora cajon = (frutaCajon cajon, kilosCajon cajon / 2)

mixturadora :: Helado -> Helado -> Helado
mixturadora (UnHelado nombre1 temperatura1 proporcion1) (UnHelado nombre2 temperatura2 proporcion2) = UnHelado{
    nombre = nombre1 ++ '-' : nombre2,
    temperatura = min temperatura1 temperatura2,
    proporcionAgua = (proporcion1 + proporcion2) / 2
} 

--punto 3 
--inciso a


cintaTransportadora :: (Helado -> Helado) -> (Helado -> Helado) -> (Helado -> Helado) -> (Helado -> Helado)
cintaTransportadora maquina1 maquina2 maquina3 = maquina3 . maquina2 . maquina1

-- inciso b
cintaUnificadora :: ((Helado -> Helado) -> (Helado -> Helado) -> (Helado -> Helado)) -> (Helado -> Helado) -> (Helado -> Helado) -> (Helado -> Helado)
cintaUnificadora megamaquina maquina1 maquina2 = megamaquina maquina1 maquina2



--Ejercicio 4

--Estos son TESTS

--ejercicio5
produccionSerie :: BidonAgua -> [CajonFruta] -> [Helado]
produccionSerie agua = filter (not . salioMal) . map (batidora agua)

