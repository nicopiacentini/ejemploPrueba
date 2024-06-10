data Ladron = UnLadron{
    nombreLadron :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
}

type Arma = Rehen -> Rehen

type Habilidad = String

data Rehen = UnRehen{
    nombreRehen :: String,
    nivelDeComplot :: Int,
    nivelDeMiedo :: Int,
    planContraLadrones :: Plan
}

type Intimidacion = Ladron -> Rehen -> Rehen
type Plan = Rehen -> Ladron -> Ladron

