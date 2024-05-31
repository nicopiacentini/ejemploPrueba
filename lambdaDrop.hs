type Requisito = Depto -> Bool
type Busqueda = [Requisito]
type Depto = (Int, Int, Int, String)
type Persona = (String, [Busqueda])
ambientes :: Depto -> Int
ambientes (a, _,_,_) =a
superficie :: Depto -> Int
superficie (_,m2,_,_) = m2
precio :: Depto -> Int
precio (_,_,p,_)= p
barrio :: Depto -> String
barrio (_,_,_,b) = b

mail :: Persona -> String
mail persona = fst persona
busquedas :: Persona -> [Busqueda]
busquedas persona = snd persona

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between :: (Ord a) => a -> a -> a -> Bool 
between x y z = x <= z && y >= z
deptosDeEjemplo :: [Depto]
deptosDeEjemplo = [(3,80,7500,"Palermo"), (1,45,3500,"Villa Urquiza"), (2,50,5000,"Palermo"), (1,45,5500,"Recoleta")]

--1
--a
mayor :: (Ord b) => (a -> b) -> a -> a -> a
mayor f elemento1 elemento2
    | f elemento1 > f elemento2 = elemento1
    | otherwise = elemento2

menor :: (Ord b) => (a -> b) -> a -> a -> a
menor f elemento1 elemento2
    | f elemento1 < f elemento2 = elemento1
    | otherwise = elemento2

--2
--a
elem' :: (Eq a) => [a] -> a -> Bool
elem' elementos elemento = elem elemento elementos

ubicadoEn :: [String] -> Depto -> Bool
ubicadoEn barrios departamento = elem' barrios . barrio $ departamento

--b
cumpleRango :: (Num a , Ord a) => Depto -> (Depto -> a) -> a -> a -> Bool
cumpleRango depto f minimo maximo = (minimo < numeroDepto) && (maximo > numeroDepto)
    where numeroDepto = f depto

--3
--a
cumpleBusqueda ::  Busqueda -> Depto -> Bool
cumpleBusqueda [] _ = True
cumpleBusqueda (requisito : requisitos) depto = requisito depto && cumpleBusqueda requisitos depto 

--b
buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar requisitos criterio  = ordenarSegun criterio . filter (cumpleBusqueda requisitos)

--c
cumpleBusqueda' :: Depto -> Busqueda -> Bool
cumpleBusqueda' _ [] = True
cumpleBusqueda' depto (requisito : requisitos) = requisito depto && cumpleBusqueda' depto requisitos 

--4
tieneBusquedaInteresada :: Depto -> Persona -> Bool
tieneBusquedaInteresada departamento persona = or . map (cumpleBusqueda' departamento) . busquedas $ persona
mailsDePersonasInteresadas :: [Persona] -> Depto -> [String]
mailsDePersonasInteresadas personas depto = map mail . filter (tieneBusquedaInteresada depto) $ personas

--5
