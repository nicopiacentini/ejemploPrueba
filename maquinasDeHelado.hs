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
