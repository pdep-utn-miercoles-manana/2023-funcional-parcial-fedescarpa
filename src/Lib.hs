import Text.Show.Functions
import Data.List(genericLength)
import Data.Char(isUpper)
--
-- * genericLength :: Num i => [a] -> i
-- -- Esta función es exactamente igual que length, 
-- -- con la única diferencia que no devuelve un Int, sino un número 
-- -- fácil de operar con otro número que pueden o no ser enteros.
-- -- 
-- -- -- ghci> length "Luigi Mario" / 2
-- -- -- error:
-- -- --     • No instance for (Fractional Int) arising from a use of ‘/’
-- -- --     • In the expression: length "Luigi Mario" / 2
-- -- --       In an equation for ‘it’: it = length "Luigi Mario" / 2
-- -- -- ghci> genericLength "Luigi Mario" / 2
-- -- -- 5.5
--
-- * isUpper :: Char -> Bool
-- -- Esta función me dice si una letra es mayúscula o no.
-- -- 
-- -- -- ghci> isUpper 'B'
-- -- -- True
-- -- -- ghci> isUpper 'b'
-- -- -- False
--
--------------
-- Punto 01 --
--------------

data Plomero = Plomero {
  nombre :: String,
  dinero :: Float,
  reparaciones :: [Reparacion],
  herramientas :: [Herramienta]
} deriving (Show)

data Herramienta = Herramienta {
  denominacion :: String,
  material :: Material,
  precio :: Float
} deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

mario:: Plomero
mario = Plomero "Mario" 1200 [] [Herramienta "Llave Inglesa" Hierro 200, Herramienta "Martillo" Madera 20]

wario:: Plomero
wario = Plomero "Wario" 0.50 [] (map (Herramienta "Llave Francesa" Hierro) [1..])

--------------
-- Punto 02 --
--------------

tiene :: String -> Plomero -> Bool
tiene unaDenominacion = any ((== unaDenominacion) . denominacion) . herramientas

esMalvado :: Plomero -> Bool
esMalvado = (== "Wa") . take 2 . nombre

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta = (>= precio unaHerramienta) . dinero

--------------
-- Punto 03 --
--------------

esBuena :: Herramienta -> Bool
esBuena (Herramienta _          Hierro   precio) = precio > 10000
esBuena (Herramienta "Martillo" material      _) = elem material [Madera, Goma]
esBuena _                                        = False

--------------
-- Punto 04 --
--------------

comprar :: Herramienta -> Plomero -> Plomero
comprar unaHerramienta unPlomero
  | puedeComprar unaHerramienta unPlomero = perderDinero (precio unaHerramienta) . agregarHerramienta unaHerramienta $ unPlomero
  | otherwise = unPlomero

perderDinero :: Float -> Plomero -> Plomero
perderDinero unDinero = mapDinero (subtract unDinero)

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta = mapHerramientas ((:) unaHerramienta)

mapDinero :: (Float -> Float) -> Plomero -> Plomero
mapDinero f unPlomero = unPlomero { dinero = f $ dinero unPlomero }

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
mapHerramientas f unPlomero = unPlomero { herramientas = f $ herramientas unPlomero }

--------------
-- Punto 05 --
--------------

data Reparacion = Reparacion {
  descripcion :: String,
  requerimiento :: Plomero -> Bool
} deriving (Show)

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion "Filtración de agua" (tiene "Llave Inglesa")

esDificil :: Reparacion -> Bool
esDificil (Reparacion descripcion _) = length descripcion >= 50 && esUrgente descripcion

esUrgente :: String -> Bool
esUrgente caracteres = all isUpper caracteres

presupuesto :: Reparacion -> Float
presupuesto = (*3) . genericLength . descripcion

--------------
-- Punto 06 --
--------------

reparar :: Reparacion -> Plomero -> Plomero
reparar unaReparacion unPlomero
  | puedeResolver unaReparacion unPlomero = agregarReparacion unaReparacion . cambiarHerramientasSegun unaReparacion . aumentarDinero (presupuesto unaReparacion) $ unPlomero
  | otherwise = aumentarDinero 100 unPlomero

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = unPlomero { reparaciones = unaReparacion : reparaciones unPlomero }

puedeResolver :: Reparacion -> Plomero -> Bool
puedeResolver unaReparacion unPlomero = requerimiento unaReparacion unPlomero || tiene "Martillo" unPlomero && esMalvado unPlomero

cambiarHerramientasSegun :: Reparacion -> Plomero -> Plomero
cambiarHerramientasSegun unaReparacion unPlomero
  | esMalvado unPlomero     = agregarHerramienta (Herramienta "Destornillador" Plastico 0) unPlomero
  | esDificil unaReparacion = mapHerramientas (filter esBuena) unPlomero
  | otherwise               = mapHerramientas (drop 1) unPlomero

aumentarDinero :: Float -> Plomero -> Plomero
aumentarDinero unDinero = mapDinero (+ unDinero)

--------------
-- Punto 07 --
--------------

diaDeLaburo :: [Reparacion] -> Plomero -> Plomero
diaDeLaburo unosReparacions unPlomero = foldl (flip reparar) unPlomero unosReparacions

--------------
-- Punto 08 --
--------------

empleadoMasReparador :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasReparador = empleadoMas (length . reparaciones)

empleadoMasAdinerado :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasAdinerado = empleadoMas dinero

empleadoMasInvertidor :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasInvertidor = empleadoMas (sum . map precio . herramientas)

empleadoMas :: Ord b => (Plomero -> b) -> [Reparacion] -> [Plomero] -> Plomero
empleadoMas f unosReparacions = maximumBy (f . diaDeLaburo unosReparacions)

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy = foldl1 . maxBy

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y
  | f x > f y = x
  | otherwise = y
