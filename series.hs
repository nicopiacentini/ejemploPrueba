data Serie = UnaSerie{
    nombre :: String,
    actores :: [Actor],
    presupuestoAnual :: Float,
    cantidadDeTemporadas :: Int,
    ratingPromedio :: Float,
    estaCancelada :: Bool
}

data Actor = UnActor{
    nombreActor :: String,
    sueldoPretendido :: Float,
    restricciones :: [String]
}

--1

todosLosSueldos :: Serie -> Float
todosLosSueldos = sum . map sueldoPretendido . actores

estaEnRojo :: Serie -> Bool
estaEnRojo serie = (< (todosLosSueldos serie)) . presupuestoAnual $ serie

--b
esProblematica :: Serie -> Bool
esProblematica serie = (>3). cantidadDeActoresConRestricciones $ serie

cantidadDeActoresConRestricciones :: Serie -> Int
cantidadDeActoresConRestricciones = length . filter (>1) . map length . map restricciones . actores 

--2
type Productor = Serie -> Serie
eliminarActores :: Int -> Serie -> Serie
eliminarActores cantidad serie = serie{actores = drop cantidad (actores serie)}

agregarActores :: [Actor] -> Serie -> Serie
agregarActores actores' serie = serie{actores = actores' ++ actores serie}

conFavoritismos :: [Actor] -> Productor
conFavoritismos actores' = agregarActores actores' . eliminarActores 2

johnnyDepp :: Actor
johnnyDepp = UnActor "Johnny Depp" 15000000 []


helenaBonham :: Actor
helenaBonham = UnActor "Helena Bonham" 20000000 []

timBurton :: Productor
timBurton = conFavoritismos [johnnyDepp,helenaBonham]

gatopardeitor :: Productor
gatopardeitor serie = serie

modificarTemporadas :: Int -> Serie -> Serie
modificarTemporadas cantidad serie = serie{cantidadDeTemporadas = cantidad}

estireitor :: Productor
estireitor serie = modificarTemporadas (2* cantidadDeTemporadas serie) serie

desespereitor :: Productor -> Productor -> Productor
desespereitor  productor1 productor2 serie= productor1 . productor2 $ serie

cancelarSerie :: Serie -> Serie
cancelarSerie serie = serie{estaCancelada = True}

cancelaitor :: Float -> Productor
cancelaitor limite serie
    | ((estaEnRojo serie) ||) . (limite <) . ratingPromedio $ serie = cancelarSerie serie
    | otherwise = serie

--3
bienestarTemporadas :: Serie -> Int
bienestarTemporadas serie
    | cantidadDeTemporadas serie > 4 = 5
    | otherwise = (10 - cantidadDeTemporadas serie) * 2

bienestarActores :: Serie -> Int
bienestarActores serie
    | length (actores serie) < 10 = 3
    | otherwise = max 2 (10 - cantidadDeActoresConRestricciones serie)

sumaDeBienestares :: Serie -> Int
sumaDeBienestares serie = bienestarTemporadas serie + bienestarActores serie

bienestar :: Serie -> Int
bienestar serie
    | estaCancelada serie = 0
    | otherwise = sumaDeBienestares serie

--4


efectivizarla :: [Productor] -> Serie -> Serie
efectivizarla [productor] serie = productor serie
efectivizarla (productor1 : productor2 : productores) serie
    | (bienestar . productor1 $ serie) > (bienestar . productor2 $ serie) = efectivizarla (productor1 : productores) serie
    | otherwise = efectivizarla (productor2 : productores) serie 
efectivizarla [] serie = serie
lasHaceMasEfectivas :: [Productor] -> [Serie] -> [Serie]
lasHaceMasEfectivas productores = map (efectivizarla productores) 

--6
tieneSalariosOrdenados :: [Float] -> Bool
tieneSalariosOrdenados [] = True
tieneSalariosOrdenados [_] = True
tieneSalariosOrdenados (salario1 : salario2 : salarios) = salario1 > salario2 && tieneSalariosOrdenados (salario2 : salarios) 

esControvertida :: Serie -> Bool
esControvertida = not . tieneSalariosOrdenados . map sueldoPretendido . actores

--7
funcionLoca :: (Integral b, Foldable t) => (Int -> b) -> (a1 -> t a2) -> [a1] -> [Int]
funcionLoca x y = filter (even.x) . map (length.y)