
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


--B
tieneHabilidad :: String -> Chico -> Bool
tieneHabilidad hability ninio = elem hability (habilidades ninio)

esSuperMaduro :: Chico -> Bool
esSuperMaduro ninio = edad ninio > 18 && elem "Sabe Manejar" (habilidades ninio)

--2
data Chica = UnaChica {
    nombreChica :: String,
    condicion :: String
}



quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA ninia ninios 
    | null niniosQueCumplen = last ninios
    | otherwise = head niniosQueCumplen
    where niniosQueCumplen = filter (tieneHabilidad (condicion ninia)) ninios


--C seccion da rules
habilidadesProhibidas :: [String]
habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules chicos = map nombre . filter pideHabilidadesProhibidas $ chicos

pideHabilidadesProhibidas :: Chico -> Bool
pideHabilidadesProhibidas chico = not(null(intersectar (deseos chico) habilidadesProhibidas))

intersectar :: [String] -> [String] -> [String]
intersectar [] _ = []
intersectar (deseo : deseos) habilidades0
    | elem deseo habilidades0 = deseo : (intersectar deseos habilidades0)
    | otherwise = intersectar deseos habilidades0
