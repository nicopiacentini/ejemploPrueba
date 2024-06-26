data Superheroe = UnSuperheroe{
    nombreSuperheroe :: String,
    vida :: Float,
    planetaOrigenSuperheroe :: Planeta,
    artefactoPredilecto :: Artefacto,
    villanoEnemigo :: Villano
}

type Planeta = String
type Artefacto = (String , Float)
nombreArtefacto :: Artefacto -> String
nombreArtefacto = fst
danioArtefacto :: Artefacto -> Float
danioArtefacto = snd

data Villano = UnVillano{
    nombreVillano :: String,
    planetaOrigenVillano :: Planeta,
    arma :: Arma
}

type Arma = Superheroe -> Superheroe

--1
trajeIronMan :: Artefacto
trajeIronMan = ("traje iron man" , 12)
ironMan :: Superheroe
ironMan = UnSuperheroe "iron man" 100 "Tierra" trajeIronMan thanos

stormbreaker :: Artefacto
stormbreaker = ("Stormbreaker" , 0)

thor :: Superheroe
thor = UnSuperheroe "Thor Odinson" 300 "Asgard" stormbreaker loki


thanos :: Villano
thanos = UnVillano "Thanos" "Titan" guanteleteDelInfinito

loki :: Villano
loki = UnVillano "Loki Laufeyson" "Jotunheim" (cetro 20)

--2
modificarVida :: Float -> Superheroe -> Superheroe
modificarVida modificacion superheroe = superheroe{vida = max 0 (modificacion + vida superheroe)}

guanteleteDelInfinito :: Arma
guanteleteDelInfinito superheroe= modificarVida (-0.8 * (vida superheroe)) superheroe

cetro :: Float -> Arma
cetro efectividad superheroe = modificarVida (- (efectividad / 10) * vida superheroe) . aplicarSi (esTerricola) (romperSuArtefacto) $ superheroe 

aplicarSi :: (a -> Bool) -> (a -> a) -> a -> a
aplicarSi criterio funcion objeto
    | criterio objeto = funcion objeto
    | otherwise = objeto

esTerricola :: Superheroe -> Bool
esTerricola = (== "Tierra") . planetaOrigenSuperheroe

cambiarArtefactoPredilecto :: Artefacto -> Superheroe -> Superheroe
cambiarArtefactoPredilecto artefacto superheroe = superheroe{artefactoPredilecto = artefacto}

romper :: Artefacto -> Artefacto 
romper artefacto = ("machacado " ++ " " ++ nombreArtefacto artefacto , 30 + danioArtefacto artefacto)   

romperSuArtefacto :: Superheroe -> Superheroe
romperSuArtefacto superheroe = cambiarArtefactoPredilecto (romper (artefactoPredilecto superheroe)) superheroe


sonAntagonistas :: Superheroe -> Villano -> Bool
sonAntagonistas superheroe villano = esElVillanoDe superheroe villano || sonOriundosDelMismoPlaneta superheroe villano

sonOriundosDelMismoPlaneta :: Superheroe -> Villano -> Bool
sonOriundosDelMismoPlaneta superheroe villano = planetaOrigenSuperheroe superheroe == planetaOrigenVillano villano

esElVillanoDe :: Superheroe -> Villano -> Bool
esElVillanoDe superheroe villano = (nombreVillano villano ==) . nombreVillano . villanoEnemigo $ superheroe

--4
atacadoPorVillanos :: [Villano] -> Superheroe -> Superheroe
atacadoPorVillanos [] superheroe = superheroe
atacadoPorVillanos (villano : villanos) superheroe
    | esElVillanoDe superheroe villano = atacadoPorVillanos villanos superheroe
    | otherwise = atacadoPorVillanos villanos ((arma villano) superheroe)

--5
sobrevivientesDelVillano :: Villano -> [Superheroe] -> [Superheroe]
sobrevivientesDelVillano villano = map (agregarAlNombre "Super") . filter sobrevive . map (arma villano)

sobrevive :: Superheroe -> Bool
sobrevive = (> 50) . vida

agregarAlNombre :: String -> Superheroe -> Superheroe
agregarAlNombre agregado superheroe = superheroe{nombreSuperheroe = agregado ++ nombreSuperheroe superheroe}

--6
volverACasa :: [Superheroe] -> [Superheroe]
volverACasa = map descansar . sobrevivientesDelVillano thanos

descansar :: Superheroe -> Superheroe
descansar = arreglarSuArtefacto . modificarVida 30

arreglarSuArtefacto :: Superheroe -> Superheroe
arreglarSuArtefacto superheroe = cambiarArtefactoPredilecto ((arreglarArtefacto . artefactoPredilecto) superheroe) superheroe

arreglarArtefacto :: Artefacto -> Artefacto
arreglarArtefacto artefacto = ((desmachacar . nombreArtefacto) artefacto , 0)

desmachacar :: String -> String
desmachacar nombre 
    | "machacado" == take (length "machacado") nombre = drop (length "machacado") nombre
    | otherwise = nombre

--7
tieneHerramientaMachacada :: Superheroe -> Bool
tieneHerramientaMachacada = ("machacado" ==) . take (length "machacado") . nombreArtefacto . artefactoPredilecto

esDebilAnte :: [Superheroe] -> Villano -> Bool
esDebilAnte superheroes villano = (null (filter tieneHerramientaMachacada superheroes) &&) . and . map (\superheroe -> esElVillanoDe superheroe villano) $ superheroes

--8
drStrange :: Superheroe
drStrange = UnSuperheroe "Stephen Strange" 60 "Tierra" ("Capa de levitacion" , 0) thanos

clonesDedrStrange :: Int -> [Superheroe]
clonesDedrStrange n = drStrange{nombreSuperheroe = "Stephen Strange " ++ show n} : clonesDedrStrange (n + 1)

clonesInfinitosDrStrange :: [Superheroe]
clonesInfinitosDrStrange = clonesDedrStrange 1

