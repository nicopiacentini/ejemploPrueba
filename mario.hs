--1

data Plomero = UnPlomero{
    nombrePlomero :: String,
    cajaHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Float
}


data Herramienta = UnaHerramienta{
    nombreHerramienta :: String,
    precio :: Float,
    materialEmpuniadura :: Material
}

data Material = Hierro | Madera | Goma | Plastico deriving Eq

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesaHierro , martilloMadera] [] 1200

llaveInglesaHierro :: Herramienta
llaveInglesaHierro = UnaHerramienta "llave inglesa" 200 Hierro

martilloMadera :: Herramienta
martilloMadera = UnaHerramienta "martillo" 20 Madera

wario :: Plomero
wario = UnPlomero "Wario" cajaDeWario [] 0.5

llavesFrancesasInfinitas :: Float -> [Herramienta]
llavesFrancesasInfinitas n = (UnaHerramienta "llave francesa" n Hierro) : llavesFrancesasInfinitas (n + 1)

cajaDeWario :: [Herramienta]
cajaDeWario = llavesFrancesasInfinitas 1


--2

tieneHerramienta :: Herramienta -> Plomero -> Bool
tieneHerramienta herramienta = any (== nombreHerramienta herramienta) . map nombreHerramienta . cajaHerramientas

esMalvado :: Plomero -> Bool
esMalvado = (== "Wa") . take 2 . nombrePlomero

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar herramienta plomero = (<= dinero plomero) . precio $ herramienta

--3
esBuena :: Herramienta -> Bool
esBuena (UnaHerramienta "martillo" _ Madera) = True
esBuena (UnaHerramienta "martillo" _ Goma) = True
esBuena (UnaHerramienta _ presio Hierro) = presio > 10000
esBuena _ = False

--4
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta herramienta plomero
    | puedeComprar herramienta plomero = modificarDinero (- precio herramienta) . agregarHerramienta herramienta $ plomero
    | otherwise = plomero

modificarDinero :: Float -> Plomero -> Plomero
modificarDinero modificacion plomero = plomero{dinero = modificacion + dinero plomero}

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramienta plomero = plomero{cajaHerramientas = herramienta : cajaHerramientas plomero}


type Reparacion = (Plomero -> Bool , String)
descripcion :: Reparacion -> String
descripcion = snd
--5
esGrito :: String -> Bool
esGrito = all (esMayuscula)

esMayuscula :: Char -> Bool
esMayuscula letra = elem letra ['A' .. 'Z']

esComplicada :: String -> Bool
esComplicada palabra = esGrito palabra && ((> 100) . length) palabra

esDificil :: Reparacion -> Bool
esDificil = esComplicada . descripcion


presupuesto :: Reparacion -> Int
presupuesto = (* 3) . length . descripcion

