--1
import Data.List(genericLength)

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


presupuesto :: (Num a) => Reparacion -> a
presupuesto = (* 3) . genericLength . descripcion

--6
requerimiento :: Reparacion -> (Plomero -> Bool)
requerimiento = fst

puedeReparar :: Reparacion -> Plomero -> Bool
puedeReparar reparacion plomero = ((requerimiento reparacion) plomero) || esMalvado plomero && tieneHerramienta (UnaHerramienta "martillo" 0 Hierro) plomero

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion reparacion plomero
    | puedeReparar reparacion plomero = reparar reparacion plomero
    | otherwise = modificarDinero 100 plomero

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero{historialReparaciones = reparacion : historialReparaciones plomero}

perderHerramientasBuenas :: Plomero -> Plomero
perderHerramientasBuenas plomero = plomero{cajaHerramientas = filter (esBuena) (cajaHerramientas plomero)}

reparar :: Reparacion -> Plomero -> Plomero
reparar reparacion plomero
    | esMalvado plomero = modificarDinero (presupuesto reparacion) . agregarReparacion reparacion . agregarHerramienta (UnaHerramienta "destornillador" 0 Plastico) $ plomero
    | esDificil reparacion = modificarDinero (presupuesto reparacion) . agregarReparacion reparacion . perderHerramientasBuenas $ plomero
    | otherwise = plomero{cajaHerramientas = drop 1 (cajaHerramientas plomero)}

--7
type JornadaDeTrabajo = [Reparacion]
trabajarUnaJornada :: JornadaDeTrabajo -> Plomero -> Plomero
trabajarUnaJornada jornada plomero = foldr (hacerReparacion) plomero jornada


--8
aquelDeMas :: (Ord a , Eq a) => (Plomero -> a) -> Plomero -> Plomero -> Plomero
aquelDeMas condicion unplomero otroplomero
    | (condicion unplomero) > (condicion otroplomero) = unplomero
    | otherwise = otroplomero

trabajo :: Plomero -> Plomero -> Plomero
trabajo = aquelDeMas (length . historialReparaciones)

plata :: Plomero -> Plomero -> Plomero
plata = aquelDeMas dinero

inversion :: Plomero -> Plomero -> Plomero
inversion = aquelDeMas (sum . map precio . cajaHerramientas)

elDeMas :: (Plomero -> Plomero -> Plomero) -> [Plomero] -> JornadaDeTrabajo -> Plomero
elDeMas condicion plomeros jornada = foldr1 condicion . map (trabajarUnaJornada jornada) $ plomeros

elDeMasTrabajo :: JornadaDeTrabajo ->  [Plomero] -> Plomero
elDeMasTrabajo jornada plomeros = elDeMas trabajo plomeros jornada

elDeMasDinero :: JornadaDeTrabajo -> [Plomero] -> Plomero
elDeMasDinero jornada plomeros = elDeMas plata plomeros jornada

elQueMasInvirtio :: JornadaDeTrabajo -> [Plomero] -> Plomero
elQueMasInvirtio jornada plomeros = elDeMas inversion plomeros jornada