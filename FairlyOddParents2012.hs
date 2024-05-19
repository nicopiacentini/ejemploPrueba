
--punto 1
--inciso a
data Chico = UnChico{
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [String]
}

aprenderHabilidades :: [String] -> Chico -> Chico
aprenderHabilidades habilities chico = chico {habilidades = habilidades chico ++ habilities}

--inciso b


jugarNeedForSpeed :: [String]
jugarNeedForSpeed = map (\n -> "jugar need for speed " ++ show n) [1..]

esGrosoEnNeedForSpeed :: Chico -> Chico
esGrosoEnNeedForSpeed ninio = ninio {habilidades = habilidades ninio ++ jugarNeedForSpeed}




--inciso c



serMayor :: Chico -> Chico
serMayor ninio = ninio{edad = 18}

--2
wanda :: Chico -> Chico
wanda (UnChico name age habilities wishes) = UnChico{
    nombre = name,
    edad = age + 1,
    habilidades =  habilities,
    deseos = tail wishes
}

cosmo :: Chico -> Chico
cosmo ninio = ninio{edad = div (edad ninio) 2}

muffinMagico :: Chico -> Chico
muffinMagico ninio = ninio{deseos = []}