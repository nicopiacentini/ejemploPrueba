--1
data Turista = UnTurista{
    cansancio :: Int,
    stress :: Int,
    estaAcompaniado :: Bool,
    idiomas :: [String]
}

ana :: Turista
ana = UnTurista 0 21 True ["espaniol"]

betho :: Turista
betho = UnTurista 15 15 False ["aleman"]

cathi :: Turista
cathi = UnTurista 15 15 False ["aleman" , "catalan"]

--2

type Excursion = Turista -> Turista
irALaPlaya :: Excursion
irALaPlaya turista
    | estaAcompaniado turista = modificarStress (-1) turista 
    | otherwise = modificarCansancio (-5) turista

modificarCansancio :: Int -> Turista -> Turista
modificarCansancio modificacion turista = turista{cansancio = modificacion + cansancio turista}


modificarStress :: Int -> Turista -> Turista
modificarStress modificacion turista = turista{stress = modificacion + stress turista}

aprenderIdioma :: String -> Turista -> Turista
aprenderIdioma idioma turista = turista{idiomas = idioma : idiomas turista}

estarAcompaniado :: Turista -> Turista
estarAcompaniado turista = turista{estaAcompaniado = True}

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma idioma = aprenderIdioma idioma . estarAcompaniado

apreciarPaisaje :: String -> Excursion
apreciarPaisaje paisaje turista = (`modificarStress` turista) . ((-1) * ). length $ paisaje

caminar :: Int -> Excursion 
caminar intensidad = modificarCansancio (div intensidad 4) . modificarStress (-1 * (div intensidad 4))

type Marea = String

paseoEnElBarco :: Marea -> Excursion
paseoEnElBarco marea turista
    | marea == "fuerte" = modificarStress 10 . modificarCansancio 6 $ turista
    | marea == "moderada" = turista
    | marea == "tranquila" = apreciarPaisaje "mar" . salirAHablarUnIdioma "aleman" $ turista
    | otherwise = error "no hay mar"


hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion turista = modificarStress (- div (10 * stress (excursion turista)) 100) . excursion $ turista

--b
type Indice = Turista -> Int

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = indice turista - (indice . hacerUnaExcursion excursion) turista

--c
esEducativa :: Turista -> Excursion -> Int
esEducativa turista excursion = deltaExcursionSegun (length . idiomas) turista excursion


esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (> 3) . deltaExcursionSegun stress turista 

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)


--3
type Tour = [Excursion]
--a
hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = modificarStress (length tour) . foldr (\excursion tourist -> hacerUnaExcursion excursion tourist) turista $ tour


--b
esConvincente :: Tour -> Turista -> Bool
esConvincente tour turista = any (esDesestresante turista) . filter (\excursion -> estaAcompaniado . excursion $ turista) $ tour

--c
espiritualidad :: Tour -> Turista -> Int
espiritualidad tour turista = cansancio turista - (cansancio . hacerTour tour) turista + stress turista - (stress . hacerTour tour) turista
efectividad :: [Turista] -> Tour -> Int
efectividad turistas tour = sum . map (espiritualidad tour) . filter (esConvincente tour) $ turistas


--4

--a
playasInfinitas :: Tour
playasInfinitas = repeat irALaPlaya

--