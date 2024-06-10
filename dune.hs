data Fremen = UnFremen{
    nombre :: String,
    toleranciaEspecia :: Int,
    titulos ::[String],
    cantidadReconocimientos :: Int
} 
type Tribu = [Fremen]

--1
agregarReconocimiento :: Fremen -> Fremen
agregarReconocimiento fremen = fremen{cantidadReconocimientos = cantidadReconocimientos fremen + 1}

esCandidato :: Fremen -> Bool
esCandidato fremen= (tieneTitulo "Domador" fremen &&) . (> 100) . toleranciaEspecia $ fremen

hayCandidato :: Tribu -> Bool
hayCandidato = any (esCandidato)

tieneTitulo :: String -> Fremen -> Bool
tieneTitulo titulo = any (== titulo) . titulos

elElegido :: Tribu -> Fremen
elElegido = elDeMasReconocimientos . filter (esCandidato)

elDeMasReconocimientos :: Tribu -> Fremen
elDeMasReconocimientos tribu = foldr1 (tieneMasReconocimientos) tribu

tieneMasReconocimientos :: Fremen -> Fremen -> Fremen
tieneMasReconocimientos fremen1 fremen2
    | cantidadReconocimientos fremen1 > cantidadReconocimientos fremen2 = fremen1
    | otherwise = fremen2



--2

data GusanoDeArena = UnGusanoDeArena{
    longitud :: Int,
    nivelDeHidratacion :: Int,
    descripcion :: String
}

aparearse :: GusanoDeArena -> GusanoDeArena -> GusanoDeArena
aparearse gusano1 gusano2 = UnGusanoDeArena{
    longitud = max (longitud gusano1) (longitud gusano2),
    nivelDeHidratacion = 0,
    descripcion = descripcion gusano1 ++ " - " ++ descripcion gusano2
}

seApareanGusanos :: [GusanoDeArena] -> [GusanoDeArena] -> [GusanoDeArena]
seApareanGusanos _ [] = []
seApareanGusanos [] _ = []
seApareanGusanos (gusano1 : unosGusanos) (gusano2 : otrosGusanos) = aparearse gusano1 gusano2 : seApareanGusanos unosGusanos otrosGusanos


--3
type Mision = GusanoDeArena -> Fremen -> Fremen
realizacionColectivaDeMision :: Mision -> GusanoDeArena -> Tribu -> Tribu
realizacionColectivaDeMision mision gusano tribu = map (mision gusano) tribu 


seMantieneElegido :: Mision -> GusanoDeArena -> Tribu -> Bool
seMantieneElegido mision gusano tribu = (nombre . elElegido $ tribu) == ((nombre . elElegido . (realizacionColectivaDeMision mision gusano)) tribu )


