--1

data Plomero = UnPlomero{
    nombrePlomero :: String,
    cajaHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Float
}
type Reparacion = (Plomero -> Bool , String)

data Herramienta = UnaHerramienta{
    nombreHerramienta :: String,
    precio :: Float,
    materialEmpuniadura :: Material
}

data Material = Hierro | Madera | Goma | Plastico

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

