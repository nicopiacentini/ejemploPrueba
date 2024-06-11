--Primera parte
--1
data Persona = UnaPersona{
    edad :: Int,
    experiencia :: Int,
    inventario :: [String]
}

data Criatura = UnaCriatura{
    peligrosidad :: Int,
    comoDesaparece :: Persona -> Bool
}

--2
modificarExperiencia :: Int -> Persona -> Persona
modificarExperiencia nuevaExperiencia persona = persona{experiencia = nuevaExperiencia}
enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura
    | (comoDesaparece criatura) persona = modificarExperiencia (peligrosidad criatura + experiencia persona) persona
    | otherwise = modificarExperiencia (1 + experiencia persona) persona


--3
experienciaDePelearMucho :: Persona -> [Criatura] -> Int
experienciaDePelearMucho persona criaturas = experiencia . foldr (\creature person -> enfrentarCriatura person creature) persona $ criaturas


--b
noPuedeHacerNada :: Persona -> Bool
noPuedeHacerNada _ = False
siemprePiedras :: Criatura
siemprePiedras = UnaCriatura 0 noPuedeHacerNada

tieneItem :: String -> Persona -> Bool
tieneItem item = elem item . inventario

gnomos :: Int -> Criatura
gnomos cantidad = UnaCriatura (2 ^ cantidad) (tieneItem "sopladorDeHojas")


fantasma :: Int -> (Persona -> Bool) -> Criatura
fantasma categoria caracteristicaPersona = UnaCriatura (10 * categoria) caracteristicaPersona


--b
menosDe13YtrajeDeOveja :: Persona -> Bool
menosDe13YtrajeDeOveja persona = ((tieneItem "traje De Oveja" persona) &&) . (< 13) . edad $ persona

criaturitasDelSenior :: [Criatura]
criaturitasDelSenior = [siemprePiedras , fantasma 3 (menosDe13YtrajeDeOveja) , gnomos 10 , fantasma 1 ((>10) . experiencia) ]
laRePelea :: Persona -> Int
laRePelea persona = experienciaDePelearMucho persona criaturitasDelSenior 



--SEGUNDA PARTE DEL PARCIAL

--1
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = []
zipWithIf funcion condicion (x : xs) (y : ys)
    | condicion y = (funcion x y) : (zipWithIf funcion condicion xs ys)
    | otherwise = y : (zipWithIf funcion condicion (x : xs) ys)

--2
abecedarioDesde :: Char -> String
abecedarioDesde letra = [letra .. 'z'] ++ takeWhile (/=letra) ['a' .. letra]

abc :: String
abc = ['a'..'z']

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraDesencriptar = last. take (1 + length(takeWhile (/=letraDesencriptar) (abecedarioDesde letraClave))) $ abc

--c
esLetra :: Char -> Bool
esLetra caracter = elem caracter abc

desencriptarTexto :: Char -> String -> String
desencriptarTexto letraClave textoEncriptado = zipWithIf (desencriptarLetra) esLetra (abecedarioDesde letraClave) textoEncriptado

--d
todasLasDesencripciones :: String -> [String]
todasLasDesencripciones encriptado = map (`desencriptarTexto` encriptado) abc 

