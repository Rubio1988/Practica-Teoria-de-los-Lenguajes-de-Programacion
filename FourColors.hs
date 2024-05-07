module FourColors where
---------------------------------
--- PRIMERA FASE: ADJACENCIES ---
---------------------------------

--- Tipos de datos
type Zone      = Int
type Row       = [Zone]
type Map       = [Row]
type Adjacency = (Zone,[Zone])

--- Función adjacency
--- Obtiene todas las adyacencias del mapa
adjacencies :: Map -> [Adjacency]
adjacencies map = aux map []

aux :: Map ->[Adjacency] -> [Adjacency]
aux (x1:[]) l = compara x1 [] l
aux (x1:x2:xs) l = aux (x2:xs) (compara x1 x2 l) 

compara :: Row -> Row -> [Adjacency] -> [Adjacency]
compara [] [] ad = ad
compara (x1:x2:xs) [] ad
  | x1>x2 = compara (x2:xs) [] (insertar (x1,[x2]) ad) 
  | x1<x2 = compara (x2:xs) [] (insertar (x2,[x1]) ad) 
  | otherwise = compara (x2:xs) [] ad 
compara (x1:[]) [] ad = ad
compara (x1:[]) (y1:[]) ad
  | x1>y1 = insertar (x1,[y1]) ad
  | x1<y1 = insertar (y1,[x1]) ad
  | otherwise = ad
compara (x1:x2:xs) (y1:y2:ys) ad
  | x1<y1 && x1<x2 = compara (x2:xs)(y2:ys) (insertar (y1,[x1]) (insertar (x2,[x1]) ad))
  | x1<y1 && x1>x2 = compara (x2:xs)(y2:ys) (insertar (y1,[x1]) (insertar (x1,[x2]) ad))
  | x1<y1 && x1==x2 = compara (x2:xs)(y2:ys) (insertar (y1,[x1]) ad)
  | x1>y1 && x1<x2 = compara (x2:xs)(y2:ys) (insertar (x2,[x1]) (insertar (x1,[y1]) ad))
  | x1>y1 && x1>x2 = compara (x2:xs)(y2:ys) (insertar (x1,[x2]) (insertar (x1,[y1]) ad))
  | x1>y1 && x1==x2 = compara (x2:xs)(y2:ys) (insertar (x1,[y1]) ad)
  | x1==y1 && x1<x2 = compara (x2:xs)(y2:ys) (insertar (x2,[x1]) ad) 
  | x1==y1 && x1>x2 = compara (x2:xs)(y2:ys) (insertar (x1,[x2]) ad)
  | x1==y1 && x1==x2 = compara (x2:xs)(y2:ys) ad 
  |otherwise =compara (x2:xs)(y2:ys) ad

insertar :: Adjacency -> [Adjacency] -> [Adjacency]
insertar (x1,(x2:xs)) ((y1,(y2:ys)):restolista)
  | x1<y1 = (x1,(x2:xs)):((y1,(y2:ys)):restolista)
  | x1>y1 = (y1,(y2:ys)):(insertar (x1,(x2:xs)) (restolista))
  | x1==y1 = (y1,(insertarzona x2 (y2:ys))):(restolista)
insertar (x1,(x2:xs)) restolista = (x1,(x2:xs)):restolista

insertarzona :: Zone -> Row -> Row
insertarzona x1 (y1:ys)
  | x1<y1 = x1:(y1:ys)
  | x1>y1 = y1:(insertarzona x1 ys)
  | x1==y1 = y1:ys
insertarzona x1 ys = x1:ys
---------------------------
--- SEGUNDA FASE: COLOR ---
---------------------------

--- Tipos de datos
data Color    = Red | Green | Blue | Yellow
  deriving (Enum,Eq,Show)
type Solution = [Color] 
type Node     = ([Adjacency],Zone,Zone,Solution)

--- Función color
--- Busca una solución para colorear un mapa con un máximo de cuatro colores
---   -Si existe solución, devuelve un String con la solución
---   -Si no existe solución, devuelve un String indicando que no existe
color :: Map -> String
color map = takeFirst (solution map)
  where --- Función takeFirst
        --- Devuelve la solución contenida en el primer elemento de una lista de nodos solución
        --- Si la lista es vacía, devuelve una cadena indicando que no hay soluciones
        takeFirst :: [Node] -> String
        takeFirst []             = "No solution!\n"
        takeFirst ((_,_,_,c):ss) = "Solution: " ++ (show c) ++ "\n"

--- Función solution
--- Devuelve TODOS los nodos solución del problema
solution :: Map -> [Node]
solution map = bt esSol comp (initialNode map)

--- Función del esquema de Backtracking
--- Esta función aplica el esquema de Backtracking a cualquier problema
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt esSol comp node
  | esSol node = [node]
  | otherwise  = concat (map (bt esSol comp) (comp node))

--- Función colorList
--- Devuelve la enumeración de todos los elementos del tipo Color
colorList :: [Color]
colorList = [toEnum 0::Color ..]

--- Función esSol
--- Comprueba si un nodo es solución o no
esSol :: Node -> Bool
esSol (ady,long,zona,sol)
  |ady==[] && length sol==long && zona==long+1 = True
  |otherwise = False

--- Función comp
--- Calcula las compleciones del nodo actual coloreando la zona actual con los colores utilizables
comp :: Node -> [Node] 
comp (((x1,(x2:xs)):ls),long,zona,sol)
  | x1==zona = hijos1 ((ls),long,zona+1,sol) (posiblesColores colorList (coloresUsados (x2:xs) sol))
  | otherwise = hijos1 (((x1,(x2:xs)):ls),long,zona+1,sol)  colorList 
   
hijos1 :: Node -> Solution -> [Node] 
hijos1 (a,long,zona,sol) [] = []
hijos1 (a,long,zona,sol) (y1:ys) = (a,long,zona,sol++[y1]):(hijos1 (a,long,zona,sol) ys)

coloresUsados :: Row -> Solution -> Solution
coloresUsados [] colores = []
coloresUsados (x1:[]) colores = colores!!num:[]
  where
    num = x1-1
coloresUsados (x1:x2:xs) colores = colores!!num:(coloresUsados (x2:xs) colores)
  where
    num = x1-1

posiblesColores :: Solution -> Solution ->Solution
posiblesColores []  colorsUsed = []
posiblesColores (color1:colores) colorsUsed
  | color1 `elem` colorsUsed =  posiblesColores (colores) colorsUsed
  | otherwise = color1:( posiblesColores (colores) colorsUsed)

--- Función initialNode
--- Construye el nodo inicial para comenzar el Backtracking
initialNode :: Map -> Node
initialNode mapa = (adys,ultZona adys,1,[])
 where 
   adys = adjacencies mapa 
   
ultZona:: [Adjacency] -> Zone
ultZona [] = 1
ultZona adyacencias = ultAdya (last adyacencias)

ultAdya :: Adjacency -> Zone
ultAdya (x1,(x2:xs)) = x1