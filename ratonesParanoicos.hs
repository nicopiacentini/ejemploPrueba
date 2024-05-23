type Requisito = String -> Bool

empiezaCon :: Char -> Requisito
empiezaCon letra = (== letra) . head

isUpper :: Char -> Bool
isUpper x = elem x ['A'..'Z']

isDigit :: Char -> Bool
isDigit x = elem x ['0'..'9']
--1
contieneA :: String -> Requisito
contieneA palabrita palabrota = take (length palabrita) palabrota == palabrita || contieneA palabrita (drop 1 palabrota)

numeros :: [Int]
numeros = [1,2,3,4,5,6,7,8,9]
--2
tieneAlMenosUnNumero :: Requisito
tieneAlMenosUnNumero palabra = any (\caracter -> isDigit caracter) palabra

mayusculas :: String
mayusculas = ['A' .. 'Z']
--3
tieneXMayusculas :: Int -> Requisito
tieneXMayusculas cantidad palabra = (== cantidad) . length . filter (\caracter -> isUpper caracter) $ palabra

--4
diccionario :: [String]
diccionario = ["aaron", "abaco", "abecedario", "baliente", "beligerante"]

esIndeducible :: Requisito
esIndeducible palabra = null . filter (\palabra0 -> contieneA palabra0 palabra) $ diccionario


mayorA :: Int -> Requisito
mayorA numero = (> numero) . length


--B

type Usuario = (String , String)
type Aplicacion = ([Usuario], [Requisito] , Encriptado)

type Encriptado = String -> String
cesarN :: Int -> Encriptado
cesarN _ [] = []
cesarN numero (letra : letras) =   (toEnum . (+ 3) . fromEnum $ letra) : cesarN numero letras

textoHashAux :: String -> Int
textoHashAux [] = 0
textoHashAux (letra : letras) =  fromEnum letra + textoHashAux letras

textoHash :: Encriptado
textoHash palabra = show . textoHashAux $ palabra

puedeUsar :: String -> Aplicacion -> Bool
puedeUsar password (_ , requisitos , _) = and . map (\requisito -> requisito password) $ requisitos

registrar :: String -> String -> Aplicacion -> Aplicacion
registrar nombreUsuario passwordUsuario (usuarios , requisitos , encriptacion) 
    | puedeUsar passwordUsuario (usuarios , requisitos , encriptacion) = ((nombreUsuario,encriptacion passwordUsuario) : usuarios , requisitos , encriptacion)
    | otherwise                                      = (usuarios , requisitos , encriptacion)

paradigma :: Aplicacion
paradigma = ([] ,[mayorA 6 , not . empiezaCon 't'] , cesarN 4)

usuariosFacebuttN :: Int -> [Usuario]
usuariosFacebuttN n= ("usuario" ++ show n , textoHash ("la passWord dificil" ++ show n)) : usuariosFacebuttN (n + 1)

usuariosFacebutt :: [Usuario]
usuariosFacebutt = usuariosFacebuttN 1

noEncripta :: Encriptado
noEncripta palabra = palabra

faceButt :: Aplicacion
faceButt = (usuariosFacebutt , [] , noEncripta)
