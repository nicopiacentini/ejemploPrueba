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
loki = UnVillano "Loki Laufeyson" "Jotunheim" (centro 20)

